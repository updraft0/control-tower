package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.page.map.MapAction
import org.updraft0.controltower.protocol.MapRole

import java.time.Instant

trait MapViewContext:
  def actions: WriteBus[MapAction]
  def mapRole: Signal[MapRole]
  def staticData: SystemStaticData
  def now: Signal[Instant]
