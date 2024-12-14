import scala.annotation.{tailrec, targetName}

val shouldDisplayEaster = false

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val isTestingData = inputLines.size match
      case value if value > 12 => false
      case value => true

    given Dimensions = isTestingData match
      case false  => Dimensions(101, 103)
      case true => Dimensions(11, 7)

    val guards = inputLines.collect:
      case s"p=${x},${y} v=${vx},${vy}" =>
        val List(x0, y0, vX, vY) = List(x, y, vx, vy).map(_.toInt)
        Guard(Position(x0, y0), Velocity(vX, vY))

    val part1 = guards.flatMap(_.quadrantAfter(100)).groupMapReduce(identity)(_ => 1)(_ + _).values.product

    val part2 = isTestingData match
      case true => 0
      case false => searchEasterEgg(guards)

    val result1 = s"$part1"
    val result2 = s"$part2"

    (s"$result1", s"$result2")

case class Dimensions(wideness: Int, tallness: Int)

@tailrec
def areAllDifferent(all: Seq[Guard]): Boolean =
  all match
    case Nil => true
    case head :: tail => ! tail.exists(_.position == head.position) && areAllDifferent(tail)

def display(guards: Seq[Guard])(using dimensions: Dimensions): Unit =
  val guardsMap = guards.map(g => (g.position.x, g.position.y) -> true).toMap
  for
    y <- 0 until dimensions.wideness
    x <- 0 until dimensions.wideness
  do
    if (x == 0)
      println()
    guardsMap.get((x, y)) match
      case Some(true) => print("#")
      case _ => print(" ")

  println()

@tailrec
def searchEasterEgg(guards: Seq[Guard], moves: Int = 1)(using dimensions: Dimensions): Int =
  val updatedGuards = guards.map(_.move)
  if (areAllDifferent(updatedGuards))
    if (shouldDisplayEaster)
      display(updatedGuards)
    moves
  else
    searchEasterEgg(updatedGuards, moves + 1)


case class Guard(position: Position, velocity: Velocity)(using dimensions: Dimensions):
  def move: Guard = this.copy(position = this.position + velocity).normalised

  private def normalised: Guard =
    def norm(pos: Int, size: Int): Int =
      pos % size match
        case modulo if modulo >= 0 => modulo
        case modulo => modulo + size

    this.copy(position = Position(norm(position.x, dimensions.wideness), norm(position.y, dimensions.tallness)))

  lazy val quadrant: Option[Int] = this.position.quadrant(dimensions.wideness, dimensions.tallness)

  def quadrantAfter(moves: Int): Option[Int] =
    this.copy(velocity = this.velocity * moves).move.quadrant


case class Position(x: Int, y: Int):
  def quadrant(nbTilesWide: Int, nbTilesHeight: Int): Option[Int] =
    (x, y) match
      case (valX, valY) if valX < nbTilesWide / 2  && valY < nbTilesHeight / 2 => Some(1)
      case (valX, valY) if valX < nbTilesWide / 2 && valY > nbTilesHeight / 2 => Some(3)
      case (valX, valY) if valX > nbTilesWide / 2 && valY < nbTilesHeight / 2 => Some(2)
      case (valX, valY) if valX > nbTilesWide / 2 && valY > nbTilesHeight / 2 => Some(4)
      case _ => None

  @targetName("add")
  def +(velocity: Velocity): Position = Position(x + velocity.vx, y + velocity.vy)

case class Velocity(vx: Int, vy: Int):
  @targetName("mult")
  def *(moves: Int): Velocity = Velocity(vx*moves, vy*moves)

