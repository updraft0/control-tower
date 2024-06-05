package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.page.map.MapAction
import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.protocol.{MapRole, UserPreferences}

import java.time.Instant

trait MapViewContext:
  def actions: WriteBus[MapAction]
  def characterId: Signal[CharacterId]
  def mapRole: Signal[MapRole]
  def staticData: SystemStaticData
  def now: Signal[Instant]
  def userPreferences: Signal[UserPreferences]
