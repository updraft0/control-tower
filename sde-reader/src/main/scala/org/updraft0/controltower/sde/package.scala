package org.updraft0.controltower

import zio.{parser as _, *}
import zio.stream.ZStream

import java.nio.file.Path

package object sde:
  /** Read the SDE export from `sde.zip` and parse its entries
    */
  def read(sdePath: Path, parallel: Int = 4): ZStream[Any, parser.Error, ExportedData] =
    zip
      .readEntries(sdePath)
      .mapError(parser.Error.Zip(_))
      .mapZIOPar(parallel)(parser.parse)
      .collectSome
      .provideLayer(yaml.YAML.layer)

  /** Group the results of the SDE export in preparation for e.g. loading into a db
    */
  def group(in: ZStream[Any, parser.Error, ExportedData]): ZStream[Any, parser.Error, GroupedExport] =
    groupBySolarSystems(in)
