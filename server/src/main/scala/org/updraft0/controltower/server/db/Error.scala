package org.updraft0.controltower.server.db

import java.sql.SQLException

enum Error:
  case Database(cause: SQLException)
  case Unknown(cause: Throwable)
