package org.updraft0.controltower.server

import zio.Chunk

extension [A](v: Option[A])
  inline def toChunk: Chunk[A] = v match
    case None    => Chunk.empty[A]
    case Some(a) => Chunk.single(a)
