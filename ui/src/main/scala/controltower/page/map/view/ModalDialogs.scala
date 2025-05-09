package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.component.Modal
import controltower.db.ReferenceDataStore
import controltower.page.map.MapAction
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.*

import scala.concurrent.ExecutionContext.Implicits.global

private[map] def removeSystemConfirm(system: MapSystemSnapshot, actions: WriteBus[MapAction], onClose: Observer[Unit])(
    using rds: ReferenceDataStore
) =
  Modal.showConfirmation(
    "Confirm removal",
    span(
      child.text <--
        Signal
          .fromFuture(rds.systemForId(system.system.systemId))
          .map(_.flatten.map(_.name).getOrElse(s"?? ${system.system.systemId}"))
          .map(solarName =>
            system.system.name.map(n => s"Remove system $n ($solarName)?").getOrElse(s"Remove system $solarName?")
          )
    ),
    actions.contramap(_ => MapAction.Remove(system.system.systemId)),
    isDestructive = true,
    onClose = onClose
  )

private[map] def removeMultipleSystems(
    systemIds: Array[SystemId],
    actions: WriteBus[MapAction],
    onClose: Observer[Unit]
) =
  Modal.showConfirmation(
    "Remove multiple systems",
    span(s"Remove ${systemIds.length} selected systems from map?"),
    actions.contramap(_ => MapAction.RemoveMultiple(systemIds)),
    isDestructive = true,
    onClose = onClose
  )

private[map] def removeConnection(
    conn: MapWormholeConnectionWithSigs,
    actions: WriteBus[MapAction],
    onClose: Observer[Unit]
)(using
    rds: ReferenceDataStore
) =
  Modal.showConfirmation(
    "Confirm removal",
    span(
      child.text <-- Signal
        .fromFuture((for
          fromSystem <- rds.systemForId(conn.connection.fromSystemId)
          toSystem   <- rds.systemForId(conn.connection.toSystemId)
        yield fromSystem.map(_.name) -> toSystem.map(_.name)).map {
          case (Some(fromName), Some(toName)) => Some(s"Remove connection from $fromName to $toName?")
          case _                              => None
        })
        .map(
          _.flatten.getOrElse(
            s"Remove connection between $$${conn.connection.fromSystemId} to $$${conn.connection.toSystemId}"
          )
        )
    ),
    actions.contramap(_ => MapAction.RemoveConnection(conn.connection.id)),
    isDestructive = true,
    onClose = onClose
  )
