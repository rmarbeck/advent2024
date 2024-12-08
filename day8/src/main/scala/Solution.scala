import scala.annotation.tailrec

type Grid = IArray[IArray[Char]]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given grid: Grid = IArray.from(inputLines.map(IArray.from))

    val antennasPositionsGrouped =
      (for
        x <- grid.indices; y <- grid(0).indices
        currentAntenna = grid(x)(y); if currentAntenna != '.'
      yield
        Antenna(Position(x, y), currentAntenna)
        ).groupMap(_.value)(_.position).values.filter(_.size >= 2)

    val (antinodesPart1, antinodesPart2) =
      antennasPositionsGrouped.foldLeft((Set(), Set())):
        case (acc, positions) => (acc._1 ++ guessAntinodes(Part1)(positions), acc._2 ++ guessAntinodes(Part2)(positions))

    val forResult2 = antennasPositionsGrouped.flatten.toSet ++ antinodesPart2

    val result1 = antinodesPart1.size
    val result2 = forResult2.size

    (s"$result1", s"$result2")

case class Position(x: Int, y: Int):
  def isDefined(using grid: Grid): Boolean = grid.isDefinedAt(x) && grid(x).isDefinedAt(y)

  private def antiNode(to: Position, distance: Int = 2): Position =
    def calcCoord: (Int, Int) => Int = distance * _ - (distance - 1) * _
    Position(calcCoord(x, to.x), calcCoord(y, to.y))

  def findAntinodes(other: Position)(using grid: Grid): Set[Position] =  Set(this.antiNode(other), other.antiNode(this)).filter(_.isDefined)

  def findAntinodesWithResonance(distance: Int)(other: Position)(using grid: Grid): Set[Position] =
    @tailrec
    def findAntinodesWithResonanceRec(distance: Int)(other: Position, found: Set[Position]): Set[Position] =
      List(this.antiNode(other, distance), other.antiNode(this, distance)).filter(_.isDefined) match
        case Nil => found
        case newAntinodes => findAntinodesWithResonanceRec(distance + 1)(other, found ++ newAntinodes.toSet)

    findAntinodesWithResonanceRec(distance)(other, Set())

case class Antenna(position: Position, value: Char)

def guessAntinodes(part: Part)(positions: Seq[Position])(using grid: Grid): Set[Position] =
  val finder: Position => Position => Set[Position] =
    part match
      case Part1 => _.findAntinodes
      case Part2 => _.findAntinodesWithResonance(2)

  @tailrec
  def doCount(toExplore: List[Position], found: Set[Position]): Set[Position] =
    toExplore match
      case Nil | _ :: Nil => found
      case head :: tail => doCount(tail, found ++ tail.flatMap(finder.apply(_).apply(head)))

  doCount(positions.toList, Set())

enum Part:
  case Part1; case Part2
export Part._