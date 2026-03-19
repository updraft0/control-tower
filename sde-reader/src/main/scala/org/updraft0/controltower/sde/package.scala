package org.updraft0.controltower

import zio.{parser as _, *}
import zio.stream.ZStream

import java.nio.file.Path

package object sde:

  /** Read the SDE export from `sde.zip` and parse its entries
    */
  def read(sdePath: Path): ZStream[Any, parser.Error, ExportedData] =
    zip
      .readEntries(sdePath)
      .mapError(parser.Error.Zip(_))
      .flatMap(parser.parse)
