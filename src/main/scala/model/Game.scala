package model

import command.{MoveAction, TurnPlayerAction, PlayerAction}

case class Game(grid: Grid, players : List[(Player, Int, Int, Direction)]) {

  def act(command: PlayerAction): (Game, Boolean) = {

    command match {
      case TurnPlayerAction(playerId:Int, direction:Direction) => {
        val playerOption = players.find(p => p._1.id == playerId)
        playerOption match {
          case Some(playerWithPosition) => {
            val turnedPlayer :(Player, Int, Int, Direction) = playerWithPosition.copy(_4 = direction)
            (this.copy (players = players.map(p => { if(p._1.id == turnedPlayer._1.id) turnedPlayer else p  })), true)
          }
          case _ => (this, false)
        }
      }
      case MoveAction(playerId:Int) => {
        val playerOption = players.find(p => p._1.id == playerId)
        playerOption match {
          case Some(playerTuple) if isMoveToCellAllowed(getCoordinatesOfNextCell(playerTuple._2, playerTuple._3, playerTuple._4)) => {
            val cellPosition = getCoordinatesOfNextCell(playerTuple._2, playerTuple._3, playerTuple._4)
            val nextCell = grid.getCellWithCoordinates(getCoordinatesOfNextCell(playerTuple._2, playerTuple._3, playerTuple._4))
            nextCell match {
              case c:EmptyCell => {
                val newPlayers = players.map(p => { if(p._1.id == playerTuple._1.id) playerTuple.copy(_2 = cellPosition._1, _3 = cellPosition._2) else p  })
                val newGrid = grid.setCell(cellPosition,PlayerCell(playerTuple._1,playerTuple._4))
                val gridWithOldPosRemoved = newGrid.setCell((playerTuple._2,playerTuple._3),EmptyCell())
                (this.copy(players = newPlayers, grid = gridWithOldPosRemoved), true)
              }
              case c:ItemCell => {
                val newPlayer: Player = playerTuple._1.copy(inventory = c.item :: playerTuple._1.inventory )
                val newPlayers = players.map(p => { if(p._1.id == playerTuple._1.id) playerTuple.copy(_1 = newPlayer, _2 = cellPosition._1, _3 = cellPosition._2) else p  })
                val newGrid = grid.setCell(cellPosition,PlayerCell(newPlayer,playerTuple._4))
                val gridWithOldPosRemoved = newGrid.setCell((playerTuple._2,playerTuple._3),EmptyCell())
                (this.copy(players = newPlayers, grid = gridWithOldPosRemoved), true)
              }
            }
          }
          case _ => (this, false)
        }
      }
    }

  }


  def getCoordinatesOfNextCell(x:Int, y:Int, direction: Direction):(Int, Int) = {
    direction match {
      case UP() => (x, y - 1)
      case DOWN() => (x, y + 1)
      case LEFT() => (x - 1, y)
      case RIGHT() => (x + 1, y)
    }
  }

  def isMoveToCellAllowed(coord : (Int, Int)) : Boolean = {
    grid.cells(coord._1)(coord._2) match {
      case  EmptyCell() => true
      case  ItemCell(_) => true
      case _ => false
    }
  }



}
