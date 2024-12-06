import scala.annotation.tailrec
import scala.collection.mutable.{HashSet, Set as MutSet}
import scala.collection.parallel.*
import collection.parallel.CollectionConverters.ImmutableSetIsParallelizable
import scala.collection
import scala.collection.mutable

type Grid = IArray[IArray[Char]]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    import Dir._

    given grid: Grid = IArray.from(inputLines.map(IArray.from)).transpose

    val findStart =
      (for
        x <- grid.indices
        y <- grid(0).indices
        if grid(x)(y) != '.' && grid(x)(y) != '#'
      yield
        POV(Position(x, y), Dir.fromChar(grid(x)(y)))
      ).head

    val exploredPositions = walkThrough(findStart, Set(findStart.position))

    val result1 = exploredPositions.size

    val result2 = exploredPositions.par.withFilter(_ != findStart).count(isALoop(findStart, mutable.HashSet[POV](findStart))(_))

    (s"$result1", s"$result2")

case class Position(x: Int, y: Int):
  def next(dir: Dir): Position = copy(x = x + dir.moveX, y = y + dir.moveY)
  def isDefined(using grid: Grid): Boolean = grid.isDefinedAt(x) && grid(x).isDefinedAt(y)

case class POV(position: Position, dir: Dir):
  lazy val Position(x, y) = position
  def next: POV = copy(position.next(dir), dir)

  def turn: POV = copy(dir = dir.next)

@tailrec
def walkThrough(pov: POV, explored: Set[Position])(using grid: Grid): Set[Position] =
  val nextPov @ POV(nextPosition @ Position(x, y), _) = pov.next
  if (nextPosition.isDefined)
    grid(x)(y) match
      case '#' => walkThrough(pov.turn, explored)
      case _ => walkThrough(pov.next, explored + nextPov.position)
  else
    explored

@tailrec
def isALoop(pov: POV, explored: MutSet[POV])(addedConstraint: Position)(using grid: Grid): Boolean =
  val nextPOV @ POV(nextPosition @ Position(x, y), _) = pov.next
  if (explored(nextPOV))
    true
  else
    if (nextPosition.isDefined)
      (grid(x)(y), addedConstraint == nextPosition) match
        case ('#', _) | (_, true) => isALoop(pov.turn, explored)(addedConstraint)
        case _ =>  isALoop(pov.next, explored += nextPOV)(addedConstraint)
    else
      false

enum Dir(val moveX: Int, val moveY: Int):
  private case N extends Dir(0, -1)
  private case E extends Dir(1, 0)
  private case S extends Dir(0, 1)
  private case W extends Dir(-1, 0)

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