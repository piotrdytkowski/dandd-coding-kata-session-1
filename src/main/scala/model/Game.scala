package model

import command.{MoveAction, TurnPlayerAction, PlayerAction}

case class Game(grid: Grid, players : List[PlayerPosition]) {


  def act(command: PlayerAction): (Game, Boolean) = {
    command match {
      case TurnPlayerAction(playerId:Int, direction:Direction) => turnPlayer(playerId, direction)
      case MoveAction(playerId:Int) => movePlayer(playerId)
    }
  }

  private def movePlayer(playerId: Int): (Game, Boolean) = {
    val playerOption = players.find(p => p.player.id == playerId)
    playerOption match {
      case Some(playerWithGPS) if isMoveToCellAllowed(getCoordinatesOfNextCell(playerWithGPS)) => {
        val cellPosition = getCoordinatesOfNextCell(playerWithGPS)
        val nextCell = grid.getCellWithCoordinates(getCoordinatesOfNextCell(playerWithGPS))
        nextCell match {
          case c: EmptyCell => {
            val newPlayers = players.map(p => {
              if (p.player.id == playerWithGPS.player.id) playerWithGPS.copy(x = cellPosition._1, y = cellPosition._2) else p
            })
            val gridWithOldPosRemoved: Grid = updateGrid(playerWithGPS, cellPosition)
            (this.copy(players = newPlayers, grid = gridWithOldPosRemoved), true)
          }
          case c: ItemCell => {
            val newPlayer: Player = playerWithGPS.player.copy(inventory = c.item :: playerWithGPS.player.inventory)
            val newPlayers = players.map(p => {
              if (p.player.id == playerWithGPS.player.id) playerWithGPS.copy(player = newPlayer, x = cellPosition._1, y = cellPosition._2) else p
            })
            val gridWithOldPosRemoved: Grid = updateGrid(playerWithGPS, cellPosition)
            (this.copy(players = newPlayers, grid = gridWithOldPosRemoved), true)
          }
        }
      }
      case _ => (this, false)
    }
  }

  private def updateGrid(playerWithGPS: PlayerPosition, cellPosition: (Int, Int)): Grid = {
    val newGrid = grid.setCell(cellPosition, PlayerCell(playerWithGPS.player, playerWithGPS.direction))
    val gridWithOldPosRemoved = newGrid.setCell((playerWithGPS.x, playerWithGPS.y), EmptyCell())
    gridWithOldPosRemoved
  }

  private def turnPlayer(playerId: Int, direction: Direction): (Game, Boolean) = {
    players.find(p => p.player.id == playerId) match {
      case Some(playerWithPosition) => {
        val turnedPlayer: PlayerPosition = playerWithPosition.copy(direction = direction)
        val newPlayers = players.map(p => {if (p.player.id == turnedPlayer.player.id) turnedPlayer else p})
        (this.copy(players = newPlayers), true)
      }
      case _ => (this, false)
    }
  }

  def getCoordinatesOfNextCell(playerWithGPS: PlayerPosition):(Int, Int) = {
    playerWithGPS.direction match {
      case UP() => (playerWithGPS.x, playerWithGPS.y - 1)
      case DOWN() => (playerWithGPS.x, playerWithGPS.y + 1)
      case LEFT() => (playerWithGPS.x - 1, playerWithGPS.y)
      case RIGHT() => (playerWithGPS.x + 1, playerWithGPS.y)
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
