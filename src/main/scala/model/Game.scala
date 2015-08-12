package model

import command.{MoveAction, TurnPlayerAction, PlayerAction}

case class Game(grid: Grid, players : List[PlayerPosition]) {


  def act(command: PlayerAction): (Game, Boolean) = {

    command match {
      case TurnPlayerAction(playerId:Int, direction:Direction) => {
        val playerOption = players.find(p => p.player.id == playerId)
        playerOption match {
          case Some(playerWithPosition) => {
            val turnedPlayer : PlayerPosition = playerWithPosition.copy(direction = direction)
            (this.copy (players = players.map(p => { if(p.player.id == turnedPlayer.player.id) turnedPlayer else p  })), true)
          }
          case _ => (this, false)
        }
      }
      case MoveAction(playerId:Int) => {
        val playerOption = players.find(p => p.player.id == playerId)
        playerOption match {
          case Some(playerTuple) if isMoveToCellAllowed(getCoordinatesOfNextCell(playerTuple.x, playerTuple.y, playerTuple.direction)) => {
            val cellPosition = getCoordinatesOfNextCell(playerTuple.x, playerTuple.y, playerTuple.direction)
            val nextCell = grid.getCellWithCoordinates(getCoordinatesOfNextCell(playerTuple.x, playerTuple.y, playerTuple.direction))
            nextCell match {
              case c:EmptyCell => {
                val newPlayers = players.map(p => { if(p.player.id == playerTuple.player.id) playerTuple.copy(x = cellPosition._1, y = cellPosition._2) else p  })
                val newGrid = grid.setCell(cellPosition,PlayerCell(playerTuple.player,playerTuple.direction))
                val gridWithOldPosRemoved = newGrid.setCell((playerTuple.x,playerTuple.y),EmptyCell())
                (this.copy(players = newPlayers, grid = gridWithOldPosRemoved), true)
              }
              case c:ItemCell => {
                val newPlayer: Player = playerTuple.player.copy(inventory = c.item :: playerTuple.player.inventory )
                val newPlayers = players.map(p => { if(p.player.id == playerTuple.player.id) playerTuple.copy(player = newPlayer, x = cellPosition._1, y = cellPosition._2) else p  })
                val newGrid = grid.setCell(cellPosition,PlayerCell(newPlayer,playerTuple.direction))
                val gridWithOldPosRemoved = newGrid.setCell((playerTuple.x,playerTuple.y),EmptyCell())
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
