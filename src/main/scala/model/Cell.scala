package model

import model.Item.Item

sealed abstract class Cell

case class EmptyCell() extends Cell

case class BlockedCell() extends Cell

case class ItemCell(item: Item) extends Cell