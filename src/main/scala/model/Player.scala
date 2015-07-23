package model

import model.Item.Item

case class Player(inventory: List[Item], id: Int, position: (Int, Int), direction: Direction) {
  def nextMove():(Int, Int) = {
    direction match {
      case UP() => (position._1, position._2 +1)
      case DOWN() => (position._1, position._2-1)
      case LEFT() => (position._1-1, position._2)
      case RIGHT() => (position._1+1, position._2)
    }
  }
}