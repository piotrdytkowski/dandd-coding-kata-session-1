package model

sealed abstract class Direction

case class UP() extends Direction
case class DOWN() extends Direction
case class LEFT() extends Direction
case class RIGHT() extends Direction

