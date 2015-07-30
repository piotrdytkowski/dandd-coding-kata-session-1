package model

case class Grid (cells : Array[Array[Cell]]) {


  def getCellWithCoordinates(coordinates: (Int, Int)): Cell = {
    cells(coordinates._1)(coordinates._2)
  }

  def setCell(coordinates: (Int,Int),cell: Cell): Grid = {
    val itemAtLocation = cells(coordinates._1)(coordinates._2)
    val newCells = cells.map(arrOfCells => {
      arrOfCells.map(c => if(c eq itemAtLocation) cell else c)
    })
    this.copy(cells = newCells)
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
        case PlayerCell(p,_) => {
          p.id
        }

      }).mkString("", "", "\n")
    }).mkString
  }

}
