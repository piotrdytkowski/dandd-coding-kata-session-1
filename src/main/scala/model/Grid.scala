package model

case class Grid (cells : Array[Array[Cell]]) {
  def removeItemAtCoordinates(coordinates: (Int, Int)):Grid = {
    val itemAtLocation = cells(coordinates._1)(coordinates._2)
    val newCells = cells.map(arrOfCells => {
      arrOfCells.map(cell => if(cell == itemAtLocation) EmptyCell() else cell)
    })
    this.copy(cells = newCells)
  }

  def getCellWithCoordinates(coordinates: (Int, Int)): Cell = {
    cells(coordinates._1)(coordinates._2)
  }

  override def toString: String = {
    cells(0).indices.map(i => {
      cells.map(arrOfCells => arrOfCells(i) match {
        case EmptyCell() => "_"
        case BlockedCell() => "X"
        case ItemCell(item) => item match {
          case Item.BONE => "I"
          case Item.COIN => "$"
          case Item.SCROLL => "@"
        }
      }).mkString("", "", "\n")
    }).mkString
  }


}
