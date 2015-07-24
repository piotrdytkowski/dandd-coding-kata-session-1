import command.{MoveAction, TurnPlayerAction}
import model._

object HelloJohnny extends App {

  println("Hello Johnny")

  def print(game : Game): Unit = {
    println (game.players.toString())
    println(game.grid)
  }

  val array : Array[Array[Cell]] = Array(
    Array(
      EmptyCell(), ItemCell(Item.COIN), EmptyCell()
    ),
    Array(
      EmptyCell(), EmptyCell(), EmptyCell()
    ),
    Array(
      ItemCell(Item.BONE), EmptyCell(), ItemCell(Item.SCROLL)
      )
    ,
    Array(
      ItemCell(Item.BONE), EmptyCell(), ItemCell(Item.SCROLL)
    )
  )
  val listOfCommands = List(TurnPlayerAction(1, RIGHT()), MoveAction(1), TurnPlayerAction(1, DOWN()), MoveAction(1), TurnPlayerAction(1, LEFT()), MoveAction(1))



  val game = Game(Grid(array), List(Player(List(), 1, (0, 0), UP())))

  val resultGame = listOfCommands.foldLeft(game)((g, action) => g.act(action))
  println(resultGame.grid)


}
