package org.updraft0.controltower.server.endpoints

import sttp.model.StatusCode
import org.updraft0.controltower.server.db

import java.sql.SQLException

private[endpoints] def dbError(error: db.Error): (StatusCode, String) =
  StatusCode.InternalServerError -> error.toString // FIXME

private[endpoints] def dbError(ex: SQLException): (StatusCode, String) =
  StatusCode.InternalServerError -> ex.toString // FIXME
