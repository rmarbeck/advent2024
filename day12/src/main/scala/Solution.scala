
import scala.annotation.tailrec

type Garden = Map[(Int, Int), Plant]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val grid = inputLines.map(_.toCharArray).toArray

    given map: Garden = (for
      y <- grid(0).indices
      x <- grid.indices
    yield
      (x, y) -> Plant(x, y, grid(x)(y))
      ).toMap

    val results = fencing(remaining = map.values.toSet)

    val result1 = s"${results._1}"
    val result2 = s"${results._2}"

    (s"$result1", s"$result2")

@tailrec
def fencing(exploring: Set[Plant] = Set(), explored: Set[Plant] = Set(), remaining: Set[Plant], currentValue: (Int, Int) = (0, 0))(using Garden): (Int, Int) =
  exploring.size match
    case 0 =>
      val (area, (part1, part2)) = explored.foldLeft((0, (0, 0))):
        case ((nb, perimeters), plant) => (nb + 1, perimeters + (plant.perimeterPart1, plant.perimeterPart2))
      val newValue = currentValue + (area * part1, area * part2)

      remaining.size match
        case 0 => newValue
        case _ =>
          val (singleHeadingSet, tail) = remaining.splitAt(1)
          fencing(singleHeadingSet, Set(), tail, newValue)

    case _ =>
      val (singleHeadingSet, tail) = exploring.splitAt(1)
      val currentPlant = singleHeadingSet.head
      fencing((currentPlant.siblingsConnected -- explored) ++ tail , explored + currentPlant, remaining - currentPlant, currentValue)

case class Plant(x: Int, y: Int, typeOfPlant: Char):
  import Plant._
  def perimeterPart1(using garden: Garden): Int = 4 - siblingsConnected.size
  def perimeterPart2(using garden: Garden): Int = countCorners

  private def findByDrift(driftX: Int, driftY: Int)(using garden: Garden): Option[Plant] = garden.get((x + driftX, y + driftY))
  private def isTheSameThan(drift: (Int, Int))(using garden: Garden): Boolean = findByDrift(drift._1, drift._2).map(_.typeOfPlant).contains(typeOfPlant)
  private def countCorners(using garden: Garden): Int =
    val left = isTheSameThan(leftDrift)
    val right = isTheSameThan(rightDrift)
    val top = isTheSameThan(topDrift)
    val bottom = isTheSameThan(bottomDrift)
    val diagonalTopLeft = isTheSameThan(topDrift + leftDrift)
    val diagonalTopRight = isTheSameThan(topDrift + rightDrift)

    def topLeft: Boolean =
      (left, top, diagonalTopLeft) match
        case (false, false, _) => true
        case (false, true, true) => true
        case (true, false, true) => true
        case _ => false
    def topRight: Boolean =
      (right, top, diagonalTopRight) match
        case (false, false, _) => true
        case (false, true, true) => true
        case (true, false, true) => true
        case _ => false
    def bottomLeft: Boolean = ! (left || bottom)

    def bottomRight: Boolean = ! (right || bottom)

    List(topLeft, topRight, bottomRight, bottomLeft).count(identity)

  def siblingsConnected(using garden: Garden): Set[Plant] =
    drifts.toSet.flatMap(findByDrift)
      .filter(_.typeOfPlant == typeOfPlant)

object Plant:
  val drifts @ List(leftDrift, rightDrift, topDrift, bottomDrift): List[(Int, Int)] = List((-1, 0), (1, 0), (0, -1), (0, 1))

extension (value: (Int, Int))
  def +(other: (Int, Int)): (Int, Int) = (value._1 + other._1, value._2 + other._2)