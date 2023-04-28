package org.updraft0.controltower.db.model

case class ItemCategory(id: Long, name: String, iconId: Option[Long])
case class ItemGroup(id: Long, categoryId: Long, name: String, iconId: Option[Long])
case class ItemName(id: Long, groupId: Long, name: String)
case class ItemType(id: Long, name: String, groupId: Long, description: Option[String])

case class StationService(id: Long, name: String)
