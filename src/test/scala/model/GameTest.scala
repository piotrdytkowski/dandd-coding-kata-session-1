package model

import command.TurnPlayerAction
import org.scalatest.Matchers

class GameTest extends org.scalatest.FlatSpec with Matchers {

  "A Game" should "turn the player in the given direction" in {
    val game = createGameWithSinglePlayer
    val result = game.act(TurnPlayerAction(1, LEFT()))

    result._1.players(0).direction should be (LEFT())
    result._2 should be (true)
  }

  it should "fail to turn a non-existing player" in {
    val game = createGameWithSinglePlayer
    val result = game.act(TurnPlayerAction(2, LEFT()))

    result._1 should be (game)
    result._2 should be (false)
  }

  private def createGameWithSinglePlayer(): Game = {
    Game(createEmptyGrid(), List(PlayerPosition(Player(List(), 1), 0, 0, UP())))
  }

  private def createEmptyGrid(): Grid = {
    val array : Array[Array[Cell]] = Array(
      Array(
        EmptyCell(), EmptyCell(), EmptyCell()
      ),
      Array(
        EmptyCell(), EmptyCell(), EmptyCell()
      ),
      Array(
        EmptyCell(), EmptyCell(), EmptyCell()
      )
    )

    Grid(array)
  }

}
