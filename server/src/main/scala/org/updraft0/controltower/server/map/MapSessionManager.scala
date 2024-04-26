package org.updraft0.controltower.server.map

import zio.*

trait MapSessionManager:
  def messages: ZIO[Scope, Nothing, Dequeue[MapSessionMessage]]

object MapSessionManager:
  private val HubCapacity = 512

  def layer: ZLayer[Any, Nothing, MapSessionManager] = ZLayer(
    for h <- Hub.dropping[MapSessionMessage](HubCapacity)
    yield new MapSessionManager:
      override def messages: ZIO[Scope, Nothing, Dequeue[MapSessionMessage]] = h.subscribe
  )
