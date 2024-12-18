
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

    val (scorePart1, _) = Dijkstra.solve(asGraph(authorizedPlacesPart1), start, end).get

    val authorizedPlacesPart2 = toAuthorizedPlacesAsMap(fallenBytes ++ toFallBytes)

    val (_, result2Option, _) = toFallBytes.reverse.foldLeft((authorizedPlacesPart2, None: Option[(Int, Int)], false)):
      case ((authorizedPlaces, previous, false), newByteToRemove) =>
        Dijkstra.solve(asGraph(authorizedPlaces), start, end) match
          case Some(value) => (authorizedPlaces, previous, true)
          case None => (authorizedPlaces + (newByteToRemove -> true), Some(newByteToRemove), false)
      case (acc, _) => acc

    val result2 = result2Option.map(_.toString).getOrElse("(?)").drop(1).dropRight(1)

    (s"${scorePart1}", s"$result2")

case class Summit(x: Int, y: Int):
  lazy val next: Seq[Summit] =
    List((0, 1), (0, -1), (1, 0), (-1, 0)).map((xDiff, yDiff) => Summit(x + xDiff, y + yDiff))


class GraphFromArray(val elements: Seq[Summit])(validNeighbours: Summit => Seq[Summit]) extends Graph[Summit]:
  override def getElements: Seq[Summit] = elements
  override def weightBetween(first: Data[Summit], second: Data[Summit]): Long = 1L
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: Seq[Data[Summit]]): Seq[Data[Summit]] =
    val possibleNeighbours = validNeighbours.apply(current.getElement)
    potentialNeighbours.filter: summitWithData =>
      possibleNeighbours.contains(summitWithData.getElement)


private def asGraph(authorizedPlaces: Map[(Int, Int), Boolean]): GraphFromArray =
  val locations = authorizedPlaces.keys.map(Summit(_, _)).toSeq
  GraphFromArray(locations)((summit: Summit) => summit.next)

