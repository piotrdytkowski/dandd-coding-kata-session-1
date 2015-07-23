import command.{MoveAction, TurnPlayerAction}
import model._

object HelloJohnny extends App {

  println("Hello Johnny")

  def print(game : Game): Unit = {
    println (game.players.toString())
  }

  val array : Array[Array[Cell]] = Array(
    Array(
      EmptyCell(), EmptyCell(), EmptyCell()
    ),
    Array(
      EmptyCell(), EmptyCell(), EmptyCell()
    )
  )

  val game = Game(Grid(array), List(Player(List(), 1, (0, 0), UP())))
  print(game)
  val newGame = game.act(TurnPlayerAction(1, RIGHT()))
  print(newGame)
  val moveGame = newGame.act(MoveAction(1))
  print(moveGame)

}
