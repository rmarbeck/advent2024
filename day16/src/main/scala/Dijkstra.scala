import scala.annotation.tailrec

trait Graph[T]:
  def getElements: Seq[T]
  def getListToExploreInitialisedFromStart(startingFrom: T): Seq[Data[T]] =
    val data = getElements.filterNot(_ == startingFrom).map(Data[T](_))
    Data(startingFrom, 0) +: data
  def weightBetween(first: Data[T], second: Data[T]): Long
  def getNeighboursOfIn(current: Data[T], potentialNeighbours: Seq[Data[T]]): Seq[Data[T]]

class Data[T](element: T, private var currentDistance: Long = Long.MaxValue):
  def getElement = element
  private var precedingElements: List[Data[T]] = Nil
  def getPreceding: List[Data[T]] = precedingElements
  def getCurrentDistance: Long = currentDistance

  def addDistanceAndPreceding(newDistance: Long, preceding: Data[T]): Unit =
    newDistance match
      case value if value == currentDistance => precedingElements = preceding :: precedingElements
      case value if value < currentDistance =>
        currentDistance = newDistance
        precedingElements = preceding :: Nil

  override def toString: String = s"{$element, $getCurrentDistance}"

object Dijkstra:
  def solve[T](graph: Graph[T], startingFrom: T, elementsToReach: List[T]): (Int, Int) =
    doSolve[T](graph.getListToExploreInitialisedFromStart(startingFrom).toVector, Map(), null, elementsToReach)(graph)

  @tailrec
  private def doSolve[T](toExplore: Vector[Data[T]], explored: Map[T, Data[T]], lastExplored: Data[T], elementsToReach: List[T])(implicit graph: Graph[T]): (Int, Int) =
    toExplore match
      case vec if vec.isEmpty => rewind(lastExplored)
      case _ if elementsToReach.exists(explored.contains) => rewind(lastExplored)
      case vec =>
        val best = vec.head
        val tail = vec.tail
        graph.getNeighboursOfIn(best, toExplore).foreach: neighbour =>
          val distance = best.getCurrentDistance + graph.weightBetween(best, neighbour)
          if (neighbour.getCurrentDistance >= distance)
            neighbour.addDistanceAndPreceding(distance, best)

        doSolve(tail.sortBy(_.getCurrentDistance), explored + (best.getElement -> best), best, elementsToReach)

  private def rewind[T](current: Data[T], counter: Int=0): (Int, Int) =
    @tailrec
    def nbDiff(list: List[Data[T]], seen: List[T]): Int =
      list match
        case Nil => seen.map(_.toString.split(",").take(2).mkString(",")).distinct.length
        case head :: tail =>
          nbDiff((tail ::: head.getPreceding), head.getElement :: seen)


    (current.getCurrentDistance.toInt, nbDiff(List(current), Nil))