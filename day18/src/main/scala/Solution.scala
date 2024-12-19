import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.util.Sorting

type Places = Map[(Int, Int), Boolean]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (size, nbBytes) = inputLines.size match
      case 25 => (6, 12)
      case _ => (70, 1024)

    val (fallenBytes, toFallBytes) =
      inputLines.collect:
        case s"$x,$y" => (x.toInt, y.toInt)
      .splitAt(nbBytes)

    val toAuthorizedPlacesAsMap: Seq[(Int, Int)] => Places =
      blocked => (for
        x <- 0 to size; y <- 0 to size
        if ! blocked.contains(x, y)
      yield
        (x, y) -> true
        ).toMap

    val authorizedPlacesPart1 = toAuthorizedPlacesAsMap(fallenBytes)

    val (start, end) = (Summit(0, 0), List(Summit(size, size)))

    import Ordering2.given

    val (scorePart1, _) = solve(TreeSet((0, start, Nil)), fallenBytes.map(Summit), end.head, size)// Dijkstra.solve(asGraph(authorizedPlacesPart1), start, end).get

    val authorizedPlacesPart2 = toAuthorizedPlacesAsMap(fallenBytes ++ toFallBytes)

    val (_, result2Option, _) = toFallBytes.reverse.foldLeft((authorizedPlacesPart2, None: Option[(Int, Int)], false)):
      case ((authorizedPlaces, previous, false), newByteToRemove) =>
        Dijkstra.solve(asGraph(authorizedPlaces, size), start, end) match
          case Some(value) => (authorizedPlaces, previous, true)
          case None => (authorizedPlaces + (newByteToRemove -> true), Some(newByteToRemove), false)
      case (acc, _) => acc

    val result2 = result2Option.map(_.toString).getOrElse("(?)").drop(1).dropRight(1)

    (s"${scorePart1}", s"$result2")

case class Summit(x: Int, y: Int):
  def next(maxSize: Int): Seq[Summit] =
    List((0, 1), (0, -1), (1, 0), (-1, 0)).map((xDiff, yDiff) => Summit(x + xDiff, y + yDiff)).filter:
      case Summit(x, y) => x >= 0 && y >=0 && x <= maxSize && y <= maxSize


class GraphFromArray(val elements: Seq[Summit])(validNeighbours: Summit => Seq[Summit]) extends Graph[Summit]:
  override def getElements: Seq[Summit] = elements
  override def weightBetween(first: Data[Summit], second: Data[Summit]): Long = 1L
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: Seq[Data[Summit]]): Seq[Data[Summit]] =
    val possibleNeighbours = validNeighbours.apply(current.getElement)
    potentialNeighbours.filter: summitWithData =>
      possibleNeighbours.contains(summitWithData.getElement)


private def asGraph(authorizedPlaces: Map[(Int, Int), Boolean], size: Int): GraphFromArray =
  val locations = authorizedPlaces.keys.map(Summit(_, _)).toSeq
  GraphFromArray(locations)((summit: Summit) => summit.next(size))


object Ordering2:
    /*given tsOrdering: Ordering[(Int, Summit, List[Summit])] with
      override def compare(x: (Int, Summit, List[Summit]), y: (Int, Summit, List[Summit])): Int = x._1.compare(y._1)*/
    given orderingSummit: Ordering[Summit] with
      override def compare(first: Summit, second: Summit): Int =
        first.x.compare(second.x) match
          case 0 => first.y.compare(second.y)
          case value => value

    import math.Ordered.orderingToOrdered
    given orderingSummits: Ordering[List[Summit]] with
      override def compare(first: List[Summit], second: List[Summit]): Int =
        first.headOption.compare(second.headOption)

@tailrec
def solve(toExplore: TreeSet[(Int, Summit, List[Summit])], forbidden: Seq[Summit], toReach: Summit, maxSize: Int): (Int, List[Summit]) =
  toExplore.head match
    case (distance, summit, list) if summit == toReach => (distance, list)
    case (distance, summit, list) =>
      val toAdd = summit.next(maxSize).filterNot(forbidden.contains)
      val asSet = (summit.next(maxSize).filterNot(forbidden.contains).map(sum => (distance + 1, sum, sum :: list))).toSet
      val newSet= toExplore.tail ++ (summit.next(maxSize).filterNot(forbidden.contains).map(sum => (distance + 1, sum, sum :: list))).toSet
      solve(toExplore.tail ++ (summit.next(maxSize).filterNot(forbidden.contains).map(sum => (distance + 1, sum, sum :: list))).toSet, forbidden, toReach, maxSize)

