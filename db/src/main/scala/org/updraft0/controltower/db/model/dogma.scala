package org.updraft0.controltower.db.model

case class DogmaAttributeCategory(id: Long, name: String, description: Option[String])
case class DogmaAttributeType(
    id: Long,
    categoryId: Option[Long],
    dataType: Int,
    name: String,
    description: Option[String],
    defaultValue: Double,
    unitId: Option[Int],
    iconId: Option[Long]
)
case class ItemDogmaAttribute(itemId: Long, attributeId: Long, value: Double)
