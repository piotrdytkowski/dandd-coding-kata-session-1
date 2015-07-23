package command

import model.Direction

abstract sealed class PlayerAction(playerId: Int)

case class TurnPlayerAction(playerId: Int, direction: Direction) extends PlayerAction(playerId)

case class MoveAction(playerId: Int) extends PlayerAction(playerId)
