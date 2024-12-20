import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.TreeSet
import scala.collection.parallel._

type Dimension = Int
type Goals = (Summit, Summit)

val part1MaxJumps = 2
val part2MaxJumps = 20

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given Dimension = inputLines.length

    val picoToSave = summon[Dimension] match
      case 15 => 12
      case _ => 100

    val (forbidden, start, end) = inputLines.zipWithIndex.foldLeft(Nil: List[Summit], Summit(0, 0), Summit(0, 0)):
      case (total, (line, y)) =>
        val partial = line.zipWithIndex.foldLeft((Nil, None, None): (List[Summit], Option[Summit], Option[Summit])):
          case (acc, ('#', x)) => (Summit(x, y) :: acc._1, acc._2, acc._3)
          case (acc, ('S', x)) => (acc._1, Some(Summit(x, y)), acc._3)
          case (acc, ('E', x)) => (acc._1, acc._2, Some(Summit(x, y)))
          case (acc, _) => acc
        (partial._1 ::: total._1, partial._2.getOrElse(total._2), partial._3.getOrElse(total._3))

    given Goals = (start, end)

    val (_, pathE2S) = solveMaze(forbidden).get

    val pathS2E = pathE2S.reverse

    val result1 = pathS2E.zip(pathS2E.drop(picoToSave + part1MaxJumps).tails).map:
      case (fromSummit, currentRemainingPath) => currentRemainingPath.count(fromSummit.shortCuts(2))
    .sum

    import collection.parallel.CollectionConverters.RangeIsParallelizable
    val result2 = (part1MaxJumps + 1  to part2MaxJumps).par.map:
      size => pathS2E.zip(pathS2E.drop(picoToSave + size).tails).map:
        case (fromSummit, currentRemainingPath) => currentRemainingPath.count(fromSummit.shortCuts(size))
      .sum
    .sum

    (s"$result1", s"${result2 + result1}")


case class Summit(x: Int, y: Int):
  def shortCuts(size: Int)(other: Summit): Boolean =
    (other.x - x).abs + (other.y - y).abs == size

  @targetName("addTuple")
  def +(drift: (Int, Int)): Summit = Summit(x+drift._1, y+drift._2)
  def next(using dimension: Dimension): Seq[Summit] =
    List((0, 1), (0, -1), (1, 0), (-1, 0)).map(this + _).filter:
      case Summit(x, y) => x >= 0 && y >=0 && x <= dimension && y <= dimension

  override def toString: String = s"$x,$y"

object Summit:
  given orderingSummit: Ordering[Summit] = Ordering.by(s => (s.x, s.y))
  given orderingSummits: Ordering[List[Summit]] = Ordering.by(_.headOption)

def solveMaze(forbidden: Seq[Summit])(using goals: Goals, dimension: Dimension): Option[(Int, List[Summit])] =
  solver(TreeSet((0, goals._1, List(goals._1))), forbidden.toSet, goals._2)

@tailrec
def solver(toExplore: TreeSet[(Int, Summit, List[Summit])], forbidden: Set[Summit], toReach: Summit)(using dimension: Dimension): Option[(Int, List[Summit])] =
  toExplore match
    case empty if empty.isEmpty => None
    case notEmpty =>
      notEmpty.head match
        case (distance, summit, list) if summit == toReach => Some((distance, list))
        case (distance, summit, list) =>
          solver(toExplore.tail ++ summit.next.filterNot(forbidden.contains).map(sum => (distance + 1, sum, sum :: list)), forbidden + summit, toReach)
