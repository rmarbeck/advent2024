import scala.annotation.tailrec

type Grid = Array[Array[Char]]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    import Dir._

    val grid = inputLines.map(_.toCharArray).toArray.transpose

    val findStart =
      (for
        x <- grid.indices
        y <- grid(0).indices
        if grid(x)(y) != '.' && grid(x)(y) != '#'
      yield
        Position(x, y, Dir.fromChar(grid(x)(y)))
        ).head


    given Grid = grid

    val result1 = walkThrough(findStart, Set((findStart.x, findStart.y)))
    val result2 = s""

    (s"$result1", s"$result2")

case class Position(x: Int, y: Int, dir: Dir):
  def next: Position =
    copy(x = x+dir.moveX, y= y+dir.moveY)

  def turn: Position =
    copy(dir = dir.next)

@tailrec
def walkThrough(position: Position, explored: Set[(Int, Int)])(using grid: Grid): Int =
  val (x, y) = (position.next.x, position.next.y)
  if (grid.isDefinedAt(x) && grid(x).isDefinedAt(y))
    grid(x)(y) match
      case '#' => walkThrough(position.turn, explored)
      case _ => walkThrough(position.next, explored + ((x, y)))
  else
    explored.size

enum Dir(val moveX: Int, val moveY: Int):
  case N extends Dir(0, -1)
  case E extends Dir(1, 0)
  case S extends Dir(0, 1)
  case W extends Dir(-1, 0)

  def next: Dir =
    this match
      case N => E
      case E => S
      case S => W
      case W => N


object Dir:
  def fromChar(char: Int): Dir =
    char match
      case '^' => N
      case '>' => E
      case 'v' => S
      case _ => W