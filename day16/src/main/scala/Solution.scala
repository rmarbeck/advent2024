import Dir.{Down, Left, Right, Up}

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

type Goals = (Summit, List[Summit])

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    import Dir._

     val (_, start, end) = inputLines.zipWithIndex.foldLeft(Nil: List[Summit], (0, 0), (0, 0)):
        case (total, (line, y)) =>
          def addSummit(accumulator: List[Summit])(x: Int) = List(Up, Down, Right, Left).map(Summit(x, y, _)) ::: accumulator
          val partial = line.zipWithIndex.foldLeft((Nil, None, None): (List[Summit], Option[(Int, Int)], Option[(Int, Int)])):
            case (acc, ('.', x)) => (addSummit(acc._1)(x), acc._2, acc._3)
            case (acc, ('S', x)) => (addSummit(acc._1)(x), Some((x, y)), acc._3)
            case (acc, ('E', x)) => (addSummit(acc._1)(x), acc._2, Some((x, y)))
            case (acc, _) => acc
          (partial._1 ::: total._1, partial._2.getOrElse(total._2), partial._3.getOrElse(total._3))

    val grid = inputLines.map(_.toCharArray).toArray
    val forbidden = (for
      y <- inputLines.indices; x <- inputLines.head.indices
      if grid(y)(x) == '#'
    yield
      List(Up, Down, Right, Left).map(Summit(x, y, _))
      ).flatten

    given Goals = (Summit.apply(start._1, start._2, Dir.Right), List(Up, Down, Right, Left).map(Summit(end._1, end._2, _)))

    val (result1, result2) = solveMaze(forbidden).get

    (s"$result1", s"$result2")

enum Dir:
  case Up
  case Down
  case Right
  case Left

  def orthogonal: Seq[Dir] = this match
    case Up | Down => Seq(Right, Left)
    case Left | Right => Seq(Up, Down)


case class Summit(x: Int, y: Int, dir: Dir):
  def withoutDir: (Int, Int) = (x, y)
  import Dir._

  private def step: Summit =
    dir match
      case Up => this.copy(y = y - 1)
      case Down => this.copy(y = y + 1)
      case Left => this.copy(x = x - 1)
      case Right => this.copy(x = x + 1)

  def next: Seq[Summit] =
    this.step +: this.dir.orthogonal.map(newDir => this.copy(dir = newDir))

  def weightBetween(other: Summit): Long =
    (other.x - x, other.y - y) match
      case (0, 0) => 1000
      case _ => 1

  override def toString: String = s"$x,$y,$dir"

object Summit:
  given orderingSummit: Ordering[Summit] = Ordering.by(s => (s.x, s.y, s.dir.ordinal))


def solveMaze(forbidden: Seq[Summit])(using goals: Goals): Option[(Long, Long)] =
  solver(TreeSet((0, goals._1, Summits(List(goals._1)))), forbidden.toSet, goals._2)

case class Summits private (summits: List[Summit], order: Int):
  lazy val withoutDir: List[(Int, Int)] = summits.map(_.withoutDir)
  def add(summit: Summit): Summits = Summits(summit :: summits, summit.hashCode() + order)

object Summits:
  def apply(summits: List[Summit]): Summits = Summits(summits, summits.hashCode())
  given orderingSummits: Ordering[Summits] = Ordering.by(_.order)

@tailrec
def solver(toExplore: TreeSet[(Long, Summit, Summits)], forbidden: Set[Summit], toReach: List[Summit], shortestDistance: Option[Long] = None, shortestPaths: Seq[List[(Int, Int)]] = Nil): Option[(Long, Long)] =
  toExplore match
    case empty if empty.isEmpty => None
    case notEmpty =>
      notEmpty.head match
        case (distance, summit, list) if toReach.contains(summit) =>
          shortestDistance match
            case None =>
              solver(toExplore.tail, forbidden, toReach, Some(distance), list.add(summit).withoutDir +: Nil)
            case Some(value) if value == distance =>
              solver(toExplore.tail, forbidden, toReach, shortestDistance, list.add(summit).withoutDir +: shortestPaths)
            case Some(value) => Some((value, shortestPaths.flatten.distinct.length))
        case (distance, summit, list) =>
          solver(toExplore.tail ++ summit.next.filterNot(forbidden.contains).map(sum => (distance + summit.weightBetween(sum), sum, list.add(sum))), forbidden + summit, toReach, shortestDistance, shortestPaths)
