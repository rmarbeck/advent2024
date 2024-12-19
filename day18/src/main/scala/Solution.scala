import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.TreeSet
import scala.util.Sorting

type Places = Map[(Int, Int), Boolean]

type Dimension = Int
type Goals = (Summit, Summit)

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (size: Dimension, nbBytes) = inputLines.size match
      case 25 => (6, 12)
      case _ => (70, 1024)

    given Dimension = size
    given Goals = (Summit(0, 0), Summit(size, size))

    val (fallenBytes, toFallBytes) =
      inputLines.collect:
        case s"$x,$y" => Summit(x.toInt, y.toInt)
      .splitAt(nbBytes)

    val (scorePart1, validPath) = solveMaze(fallenBytes) match
      case Some(result) => result
      case None => throw Exception("No result found for part 1")

    val result2 = solvePart2(toFallBytes, validPath,fallenBytes) match
        case Some(result) => result
        case None => throw Exception("No result found for part 2")

    (s"$scorePart1", s"$result2")

case class Summit(x: Int, y: Int):
  @targetName("addTuple")
  def +(drift: (Int, Int)): Summit = Summit(x+drift._1, y+drift._2)
  def next(using dimension: Dimension): Seq[Summit] =
    List((0, 1), (0, -1), (1, 0), (-1, 0)).map(this + _).filter:
      case Summit(x, y) => x >= 0 && y >=0 && x <= dimension && y <= dimension

  override def toString: String = s"$x,$y"

object Summit:
  given orderingSummit: Ordering[Summit] = Ordering.by(s => (s.x, s.y))
  given orderingSummits: Ordering[List[Summit]] = Ordering.by(_.headOption)

@tailrec
def solvePart2(bytesToAdd: Seq[Summit], currentValidPath: List[Summit], forbidden: Seq[Summit])(using goals: Goals, dimension: Dimension): Option[Summit] =
  bytesToAdd match
    case Nil => None
    case head :: tail =>
      currentValidPath.contains(head) match
        case false => solvePart2(tail, currentValidPath, head +: forbidden)
        case true =>
          solveMaze(head +: forbidden) match
            case None => Some(head)
            case Some(_, newValidPath) => solvePart2(tail, newValidPath, head +: forbidden)


def solveMaze(forbidden: Seq[Summit])(using goals: Goals, dimension: Dimension): Option[(Int, List[Summit])] =
  solver(TreeSet((0, goals._1, Nil)), forbidden.map(_ -> true).toMap, goals._2)

@tailrec
def solver(toExplore: TreeSet[(Int, Summit, List[Summit])], forbidden: Map[Summit, Boolean], toReach: Summit)(using dimension: Dimension): Option[(Int, List[Summit])] =
  toExplore match
    case empty if empty.isEmpty => None
    case notEmpty =>
      notEmpty.head match
        case (distance, summit, list) if summit == toReach => Some((distance, list))
        case (distance, summit, list) =>
          solver(toExplore.tail ++ summit.next.filterNot(forbidden.contains).map(sum => (distance + 1, sum, sum :: list)), forbidden + (summit -> true), toReach)

