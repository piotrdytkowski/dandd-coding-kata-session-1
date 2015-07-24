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
          case Some(player) if isMoveToCellAllowed(player.getCoordinatesOfNextCell()) => {
            val nextCell = grid.getCellWithCoordinates(player.getCoordinatesOfNextCell())
            nextCell match {
              case c:EmptyCell => {
                val newPlayers = players.map(p => { if(p.id == player.id) player.copy(position = player.getCoordinatesOfNextCell()) else p  })
                this.copy (players = newPlayers)
              }
              case c:ItemCell => {
                val newPlayers = players.map(p => { if(p.id == player.id) player.copy(position = player.getCoordinatesOfNextCell(), inventory = c.item :: player.inventory ) else p  })
                val newGrid = grid.removeItemAtCoordinates(player.getCoordinatesOfNextCell())
                this.copy(players = newPlayers, grid = newGrid)
              }
            }
          }
          case _ => ???
        }
      }
    }

  }

  def isMoveToCellAllowed(coord : (Int, Int)) : Boolean = {
    grid.cells(coord._1)(coord._2) match {
      case  EmptyCell() => true
      case  ItemCell() => true
      case _ => false
    }
  }

}
