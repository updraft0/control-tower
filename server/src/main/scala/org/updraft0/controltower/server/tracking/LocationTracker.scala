package org.updraft0.controltower.server.tracking

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.server.auth.CharacterAuth
import org.updraft0.controltower.server.Log
import org.updraft0.esi.client.{EsiClient, EsiError}
import zio.*

import java.time.Instant

enum LocationTrackingRequest:
  case AuthUpdated(auths: Chunk[CharacterAuth])
  case AuthUpdatedOne(auth: CharacterAuth)
  case AddCharacters(chars: Chunk[CharacterId])
  case RemoveCharacters(chars: Chunk[CharacterId])

enum CharacterLocationState derives CanEqual:
  case InSystem(
      system: SystemId,
      prevSystem: Option[SystemId],
      shipTypeId: Int,
      stationId: Option[Int],
      structureId: Option[Long],
      updatedAt: Instant
  )
  case Offline // TODO add login/logout times and locations too
  case NoAuth
  case ApiError
  case RateLimited(tryAt: Instant)

case class LocationUpdate(state: Map[CharacterId, CharacterLocationState])

private[tracking] case class CharacterState(
    charId: CharacterId,
    state: CharacterLocationState,
    auth: Option[CharacterAuth],
    updatedAt: Instant,
    prevState: Option[CharacterLocationState]
)
private[tracking] case class TrackerState(charState: Map[CharacterId, CharacterState])

trait LocationTracker:
  def inbound: Enqueue[LocationTrackingRequest]
  def updates: URIO[Scope, Dequeue[LocationUpdate]]

object LocationTracker:
  private val InCapacity            = 64
  private val InternalHubCapacity   = 64
  private val OnlineUpdateSeconds   = 60

  case class Config(interval: Duration, maxParallel: Int)

  def layer: ZLayer[EsiClient & CharacterAuthTracker & Config, Throwable, LocationTracker] =
    ZLayer.scoped(ZIO.serviceWithZIO[Config](c => apply(c)).tap(subscribeToAuthUpdates))

  def apply(c: Config): ZIO[Scope & EsiClient, Throwable, LocationTracker] =
    for
      // create services
      esi         <- ZIO.service[EsiClient]
      state       <- Ref.make(TrackerState(Map.empty))
      inQ         <- Queue.dropping[LocationTrackingRequest](InCapacity)
      responseHub <- Hub.bounded[LocationUpdate](InternalHubCapacity)
      // inbound messages
      _ <- inQ.take
        .flatMap(msg => handleInbound(c, state, responseHub, msg))
        .ignoreLogged
        .forever
        .forkScoped
      // process updates every interval
      _ <- refreshLocations(esi, state, responseHub, c.maxParallel)
        .timeout(c.interval)
        .ignoreLogged
        .schedule(Schedule.fixed(c.interval))
        .forkScoped
    yield new LocationTracker:
      override def inbound: Enqueue[LocationTrackingRequest]     = inQ
      override def updates: URIO[Scope, Dequeue[LocationUpdate]] = responseHub.subscribe

  private def subscribeToAuthUpdates(lt: LocationTracker) =
    for
      authTracker <- ZIO.service[CharacterAuthTracker]
      updateQ     <- authTracker.updates
      _ <- updateQ.take
        .flatMap(cas => lt.inbound.offer(LocationTrackingRequest.AuthUpdated(cas)))
        .forever
        .forkScoped
    yield ()

  /** Handles inbound requests - this is mostly pure state management
    */
  private def handleInbound(
      c: Config,
      state: Ref[TrackerState],
      out: Enqueue[LocationUpdate],
      msg: LocationTrackingRequest
  ) =
    for
      now <- ZIO.clockWith(_.instant)
      _ <- msg match
        case LocationTrackingRequest.AddCharacters(chars) =>
          state
            .update(s => s.copy(charState = chars.foldLeft(s.charState)((s, c) => updateCharAt(s, c, None, now))))
        case LocationTrackingRequest.RemoveCharacters(chars) =>
          state
            .update(s => s.copy(charState = chars.foldLeft(s.charState)((s, c) => s - c)))
        case LocationTrackingRequest.AuthUpdated(chars) =>
          state
            .update(s =>
              s.copy(charState = chars.foldLeft(s.charState)((s, c) => updateCharAt(s, c.characterId, Some(c), now)))
            )
        case LocationTrackingRequest.AuthUpdatedOne(char) =>
          state
            .update(s => s.copy(charState = updateCharAt(s.charState, char.characterId, Some(char), now)))
    yield ()

  private def updateCharAt(
      s: Map[CharacterId, CharacterState],
      charId: CharacterId,
      auth: Option[CharacterAuth],
      now: Instant
  ) =
    s.updatedWith(charId):
      case None       => Some(CharacterState(charId, CharacterLocationState.NoAuth, auth, now, None))
      case Some(prev) => Some(prev.copy(auth = auth.orElse(prev.auth), updatedAt = now))

  private def refreshLocations(
      esi: EsiClient,
      state: Ref[TrackerState],
      response: Enqueue[LocationUpdate],
      parallel: Int
  ) =
    for
      curr <- state.get
      now  <- ZIO.clockWith(_.instant)
      withAuth = curr.charState.view.filter(_._2.auth.isDefined).values
      res <- ZIO.foreachExec(withAuth)(ExecutionStrategy.ParallelN(parallel))(cs =>
        refreshLocation(esi, now, cs) @@ Log.CharacterId(cs.charId) @@ Log.BackgroundOperation("locationTracker")
      )
      next <- state.updateAndGet(ts => ts.copy(charState = res.foldLeft(ts.charState)((m, s) => updateOnRefresh(m, s))))
      locUpdate = LocationUpdate(next.charState.transform((_, s) => s.state))
      _ <- response.offer(locUpdate).when(next.charState.nonEmpty)
    yield ()

  private def updateOnRefresh(m: Map[CharacterId, CharacterState], s: CharacterState) =
    m.updatedWith(s.charId):
      case None => Some(s)
      case Some(p) =>
        Some(s.copy(auth = p.auth.orElse(s.auth))) // update auth in case it was refreshed during checking locations

  private def refreshLocation(esi: EsiClient, now: Instant, st: CharacterState): UIO[CharacterState] =
    st match
      case CharacterState(_, _, None, _, _) =>
        // no-op - with no auth there is nothing to update
        ZIO.succeed(st.copy(state = CharacterLocationState.NoAuth, updatedAt = now))
      case CharacterState(_, CharacterLocationState.Offline, _, prevAt, _)
          if now.isBefore(prevAt.plusSeconds(OnlineUpdateSeconds)) =>
        // no-op - with the character offline within the endpoint cache window there is nothing to update
        ZIO.succeed(st)
      case CharacterState(charId, _, Some(auth), _, _) if auth.expiry.isBefore(now) =>
        // cannot use a token that is expired (but character auth tracker should give us an update)
        ZIO
          .logWarning("Not refreshing character due to expired auth token")
          .as(st.copy(auth = None, state = CharacterLocationState.NoAuth, updatedAt = now))
      case CharacterState(charId, prevState, Some(auth), prevAt, _) =>
        // refresh with previous state
        doRefresh(esi, now, charId, prevState, auth)
          .tapError(ex => ZIO.logError(s"ESI error while refreshing character status: ${ex}"))
          .fold(
            {
              case EsiError.BadGateway => st // ignore bad gateway errors
              case _: EsiError.Timeout => st // ignore gateway timeouts
              case _ => st.copy(state = CharacterLocationState.ApiError, prevState = Some(prevState), updatedAt = now)
            },
            identity
          )
          .resurrect
          .tapError(ex => ZIO.logErrorCause("Seriously failed to refresh character", Cause.fail(ex)))
          .fold(
            {
              case iox: java.io.IOException if iox.getMessage.contains("GOAWAY received") =>
                // TODO look into using a different client that handles errors more gracefully?
                // ignore HTTP/2 GOAWAY as a transient error similar to the 502 errors above
                st
              case _ => st.copy(state = CharacterLocationState.ApiError, prevState = Some(prevState), updatedAt = now)
            },
            identity
          )

  private def doRefresh(
      esi: EsiClient,
      now: Instant,
      charId: CharacterId,
      prevState: CharacterLocationState,
      auth: CharacterAuth
  ) =
    (esi.getCharacterOnline(auth.token)(charId) <&>
      esi.getCharacterLocation(auth.token)(charId) <&>
      esi.getCharacterShip(auth.token)(charId))
      .map: (online, location, ship) =>
        val prevSystemId = prevState match
          case is: CharacterLocationState.InSystem => Some(is.system)
          case _                                   => None

        val newState =
          if (!online.online) CharacterLocationState.Offline
          else
            CharacterLocationState.InSystem(
              location.solarSystemId,
              prevSystemId,
              ship.shipTypeId,
              location.stationId,
              location.structureId,
              now
            )

        CharacterState(charId, newState, Some(auth), now, Some(prevState))
