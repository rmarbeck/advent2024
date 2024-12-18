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
  private var precedingElement: Option[Data[T]] = None
  def getPreceding: Option[Data[T]] = precedingElement
  def getCurrentDistance: Long = currentDistance

  def updateDistanceAndPreceding(newDistance: Long, preceding: Data[T]): Unit =
    precedingElement = Some(preceding)
    currentDistance = newDistance


  override def toString: String = s"{$element, $getCurrentDistance}"

object Dijkstra:
  def solve[T](graph: Graph[T], startingFrom: T, elementsToReach: List[T]): Option[(Int, List[T])] =
    doSolve[T](graph.getListToExploreInitialisedFromStart(startingFrom).toVector, Map(), null, elementsToReach)(graph) match
      case None => None
      case result @ Some((distance, path)) =>
        path.contains(startingFrom) match
          case true => result
          case false => None

  @tailrec
  private def doSolve[T](toExplore: Vector[Data[T]], explored: Map[T, Data[T]], lastExplored: Data[T], elementsToReach: List[T])(implicit graph: Graph[T]): Option[(Int, List[T])] =
    toExplore match
      case vec if vec.isEmpty => None
      case _ if elementsToReach.exists(explored.contains) => Some(rewind(lastExplored))
      case vec =>
        val best = vec.minBy(_.getCurrentDistance)
        val tail = vec.filterNot(_ == best)
        graph.getNeighboursOfIn(best, toExplore).foreach: neighbour =>
          val distance = best.getCurrentDistance + graph.weightBetween(best, neighbour)
          if (neighbour.getCurrentDistance >= distance)
            neighbour.updateDistanceAndPreceding(distance, best)

        doSolve(tail, explored + (best.getElement -> best), best, elementsToReach)

  private def rewind[T](current: Data[T], best: Int = 0, path: List[T] = Nil): (Int, List[T]) =
    current.getPreceding match
      case Some(preceding) =>
        if (best == 0)
          rewind(preceding, current.getCurrentDistance.toInt, current.getElement :: path)
        else
          rewind(preceding, best, current.getElement :: path)
      case _ => (best, current.getElement :: path)