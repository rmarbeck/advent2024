import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val guards = inputLines.collect:
      case s"p=${x},${y} v=${vx},${vy}" =>
        val List(x0, y0, vX, vY): List[Long] = List(x, y, vx, vy).map(_.toLong)
        Guard(Position(x0, y0), Velocity(vX, vY))

    val res = guards.flatMap(_.quadrantAfter(100, 101, 103)).groupMapReduce(identity)(_ => 1)(_ + _).values.tapEach(println).product

    val res2 = searchEasterEgg(guards, 101, 103)

    val result1 = s"$res"
    val result2 = s"$res2"

    (s"$result1", s"$result2")

@tailrec
def searchEasterEgg(guards: Seq[Guard], nbTilesWide: Int, nbTilesHeight: Int, moves: Int = 1): Int =
  val inQuadrants = guards.flatMap(_.quadrantAfter(moves, nbTilesWide, nbTilesHeight)).groupMapReduce(identity)(_ => 1)(_ + _).values
  val all @ List(up, down) = inQuadrants.grouped(2).toList
  if (all.forall(current => current.head == current.last) && up.head < down.head)
    moves
  else
    searchEasterEgg(guards, nbTilesWide, nbTilesHeight, moves + 1)

case class Guard(position: Position, velocity: Velocity):
  def quadrantAfter(moves: Int, nbTilesWide: Int, nbTilesHeight: Int): Option[Int] =
    def normalised(pos: Position): Position =
      val newX = pos.x match
        case value if value >= 0 => value % nbTilesWide
        case value => (value % nbTilesWide) match
          case 0 => 0
          case other => other + nbTilesWide

      val newY = pos.y match
        case value if value >= 0 => value % nbTilesHeight
        case value => (value % nbTilesHeight) match
          case 0 => 0
          case other => other + nbTilesHeight

      //println(s"$newX, $newY")
      Position(newX, newY)

    //println(s"${velocity * moves} : ${position} => ${position + (velocity * moves)} : ${normalised(position + (velocity * moves))}")
    normalised(position + (velocity * moves)).quadrant(nbTilesWide, nbTilesHeight)


case class Position(x: Long, y: Long):
  def quadrant(nbTilesWide: Int, nbTilesHeight: Int): Option[Int] =
    (x, y) match
      case (valX, valY) if valX < nbTilesWide / 2  && valY < nbTilesHeight / 2 => Some(1)
      case (valX, valY) if valX < nbTilesWide / 2 && valY > nbTilesHeight / 2 => Some(3)
      case (valX, valY) if valX > nbTilesWide / 2 && valY < nbTilesHeight / 2 => Some(2)
      case (valX, valY) if valX > nbTilesWide / 2 && valY > nbTilesHeight / 2 => Some(4)
      case _ => None

  def +(velocity: Velocity): Position = Position(x + velocity.vx, y + velocity.vy)

case class Velocity(vx: Long, vy: Long):
  def *(moves: Int): Velocity = Velocity(vx*moves, vy*moves)

