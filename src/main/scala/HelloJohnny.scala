import command.{PlayerAction, MoveAction, TurnPlayerAction}
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
  val eventStore = List(
    TurnPlayerAction(1, RIGHT()),
    MoveAction(1),
    TurnPlayerAction(2, LEFT()),
    MoveAction(2),
    TurnPlayerAction(2, UP()),
    MoveAction(2),
    MoveAction(1),
    TurnPlayerAction(6, DOWN()),
    TurnPlayerAction(1, DOWN()),
    MoveAction(1),
    TurnPlayerAction(1, LEFT()),
    MoveAction(1))



  val game = Game(Grid(array), List(PlayerPosition(Player(List(), 1), 0, 0, UP()), PlayerPosition(Player(List(), 2), 2,2, UP())))

  val resultGame = {
    eventStore.foldLeft((game, List[(PlayerAction, Boolean)]()))((g, action) => {
      val act: (Game, Boolean) = g._1.act(action)
      println(act._1.grid)
      println()
      (act._1, (action, act._2) :: g._2)
    })
  }

  println(resultGame._1.grid)
  println(resultGame._2.reverse.mkString("", "\n", ""))


}
