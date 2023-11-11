package controltower.component

import com.raquo.laminar.api.L.Element

trait Base:
  val el: Element

given Conversion[Base, Element] = _.el
