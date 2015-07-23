package model

import command.{MoveAction, TurnPlayerAction, PlayerAction}

case class Game(grid: Grid, players : List[Player]) {

  def act(command: PlayerAction): Game = {

    command match {
      case TurnPlayerAction(playerId:Int, direction:Direction) => {
        val playerOption = players.find(p => p.id == playerId)
        playerOption match {
          case Some(player) => {
            val turnedPlayer = player.copy(direction = direction)
            this.copy (players = players.map(p => { if(p.id == turnedPlayer.id) turnedPlayer else p  }))
          }
          case _ => ???
        }
      }
      case MoveAction(playerId:Int) => {
        val playerOption = players.find(p => p.id == playerId)
        playerOption match {
          case Some(player) if soTrue(player.nextMove()) => {
            this.copy (players = players.map(p => { if(p.id == player.id) player.copy(position = player.nextMove()) else p  }))

          }
          case _ => ???
        }
      }
    }

  }

  def soTrue(coord : (Int, Int)) : Boolean = {
    grid.cells(coord._1)(coord._2) match {
      case  c:EmptyCell => {true}
      case _ => false
    }
  }

}
