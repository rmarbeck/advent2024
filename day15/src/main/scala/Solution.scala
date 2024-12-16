import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Map

type Warehouse = mutable.Map[(Int, Int), Location]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val warehouseRaw = inputLines.filter(_.startsWith("#")).map(_.toCharArray.map(Location.from)).toArray

    val moves = inputLines.filter(_.matches("""^[<>^v]+$""")).flatMap(_.map(Directions.fromChar)).toList

    val (guardX, guardY) =
      warehouseRaw.zipWithIndex.map:
        case (line, index) => (line.indexOf(Robot), index)
      .find(_._1 != -1).get

    val result1 = {
      val guard = Guard(Position(guardX, guardY))

      given warehousePart1: Warehouse = mutable.Map(
        (for
          y <- warehouseRaw.indices
          x <- warehouseRaw(0).indices
          value = warehouseRaw(y)(x)
          if value == Wall || value == Box
        yield
          (x, y) -> value
        ): _*).withDefaultValue(Empty)

      move(guard, moves, warehousePart1, warehouseRaw.length)

      warehousePart1.filter(_._2 == Box).keys.map((x, y) => y * 100 + x).sum
    }



    val result2 = {
      val guard = Guard(Position(guardX*2, guardY))

      given warehousePart2: Warehouse = mutable.Map(
        (for
          y <- warehouseRaw.indices
          x <- 0 until warehouseRaw(0).length * 2
          value = warehouseRaw(y)(x / 2)
          if value == Wall || value == Box
        yield
          val content =
            value match
              case Box if x % 2 ==0 => BoxS
              case Box              => BoxE
              case _                => value
          (x, y) -> content
        ): _*).withDefaultValue(Empty)

      move(guard, moves, warehousePart2, warehouseRaw.length)

      warehousePart2.filter(_._2 == BoxS).keys.map((x, y) => y * 100 + x).sum
    }

    (s"$result1", s"$result2")


def display(size: Int, guard: Option[Guard] = None)(using warehouse: Warehouse): Unit =
  println()
  for
    y <- 0 until size; x <- 0 until size * 2
  do
    if (x == 0)
      println()
    if (guard.contains(Guard(Position(x, y))))
      print('@')
    else
      print(
        warehouse(x, y) match
          case Empty => '.'
          case Box => 'O'
          case BoxS => '['
          case BoxE => ']'
          case Wall => '#'
          case Robot => '@'
      )
  println()

@tailrec
def move(robot: Guard, remainingMoves: List[Directions], warehouse: Warehouse, size: Int): Warehouse =
  given Warehouse = warehouse
  remainingMoves match
    case Nil => warehouse
    case head :: tail =>
      if robot.canMove(head) then
        val (newRobot, newWarehouse) = robot.moveIt(head)
        move(newRobot, tail, newWarehouse, size)
      else
        move(robot, tail, warehouse, size)



case class Guard(position: Position):
  def canMove(direction: Directions)(using warehouse: Warehouse): Boolean = position.canMove(direction)
  def moveIt(direction: Directions)(using warehouse: Warehouse): (Guard, Warehouse) =
    val (newPosition, newWarehouse) = position.move(direction)
    (Guard(newPosition), newWarehouse)

case class Position(x: Int, y: Int):
  private def next(direction: Directions): Position =
    direction match
      case Up => this.copy(y = this.y - 1)
      case Down => this.copy(y = this.y + 1)
      case Right => this.copy(x = this.x + 1)
      case Left => this.copy(x = this.x - 1)

  private def previous(direction: Directions): Position =
    direction match
      case Up => next(Down)
      case Down => next(Up)
      case Right => next(Left)
      case Left => next(Right)

  private def markBigBoxEmpty(using warehouse: Warehouse): Unit =
    warehouse((x, y)) = Empty
    warehouse((x + 1, y)) = Empty

  @tailrec
  final def canMove(direction: Directions)(using warehouse: Warehouse): Boolean =
    val newBox @ Position(newX, newY) = next(direction)
    warehouse(newX, newY) match
      case Empty => true
      case Box => newBox.canMove(direction: Directions)
      case box @ (BoxS | BoxE) => newBox.canMoveBigBox(direction, box)
      case _ => false


  private final def canMoveBigBox(direction: Directions, box: Location)(using warehouse: Warehouse): Boolean =
    box match
      case BoxE => this.copy(x = this.x - 1).canMoveBigBox(direction, BoxS)
      case _ =>
        val nextBoxS @ Position(nextX, nextY) =
          direction match
            case Up | Down => this.next(direction)
            case _ => this.next(direction).next(direction)

        direction match
          case Up | Down =>
            (warehouse(nextX, nextY), warehouse(nextX + 1, nextY)) match
              case (Wall, _) | (_, Wall) => false
              case (Empty, Empty) => true
              case (Empty, _) =>
                nextBoxS.copy(x = x + 1).canMoveBigBox(direction, BoxS)
              case (_, Empty) =>
                nextBoxS.canMoveBigBox(direction, BoxE)
              case (BoxS, BoxE) =>
                nextBoxS.canMoveBigBox(direction, BoxS)
              case (BoxE, BoxS) =>
                nextBoxS.canMoveBigBox(direction, BoxE) && nextBoxS.copy(x = x + 1).canMoveBigBox(direction, BoxS)
              case value => throw Exception(s"Not supported : $value")
          case Left =>
            (warehouse(nextX, nextY), warehouse(nextX + 1, nextY)) match
              case (_, Wall) => false
              case (Wall, Empty) => true
              case (Empty | BoxE, _) => true
              case (BoxS, _) => nextBoxS.canMoveBigBox(direction, BoxS)
              case value => throw Exception(s"Not supported : $value, $this, $nextBoxS")
          case Right =>
            warehouse(nextX, nextY) match
              case Wall => false
              case Empty => true
              case BoxS => nextBoxS.canMoveBigBox(direction, BoxS)
              case value => throw Exception(s"Not supported : $value, $this, $nextBoxS")

  def move(direction: Directions)(using warehouse: Warehouse): (Position, Warehouse) =
    val newPosition @ Position(newX, newY) = next(direction)
    warehouse(newX, newY) match
      case Empty => (newPosition, warehouse)
      case Box =>
        val newWarehouse = newPosition.moveBox(direction)
        newWarehouse((newX, newY)) = Empty
        (newPosition, newWarehouse)
      case box @ (BoxS | BoxE) =>
        val newWarehouse = newPosition.moveBigBox(direction, box)
        (newPosition, newWarehouse)
      case _ => throw Exception("Not supported")

  @tailrec
  private def moveBox(direction: Directions)(using warehouse: Warehouse): Warehouse =
    warehouse(x, y) match
      case Empty =>
        warehouse((x, y)) = Box
        warehouse
      case Box => next(direction).moveBox(direction)
      case _ => throw Exception("Not supported")

  private def moveBigBox(direction: Directions, box: Location)(using warehouse: Warehouse): Warehouse =
    val nextS @ Position(nextX, nextY) =
      direction match
        case Up | Down => this.next(direction)
        case _ => this.next(direction).next(direction)

    box match
      case BoxE => this.copy(x = this.x - 1).moveBigBox(direction, BoxS)
      case _ =>
        direction match
          case Up | Down =>
            (warehouse(nextX, nextY), warehouse(nextX + 1, nextY)) match
              case (Empty, Empty) | (Wall, _) | (_, Wall) => ()
              case (Empty, BoxS) =>
                nextS.copy(x = x + 1).moveBigBox(direction, BoxS)
              case (BoxE, Empty) =>
                nextS.moveBigBox(direction, BoxE)
              case (BoxS, BoxE) =>
                nextS.moveBigBox(direction, BoxS)
              case (BoxE, BoxS) =>
                nextS.moveBigBox(direction, BoxE)
                nextS.copy(x = x + 1).moveBigBox(direction, BoxS)
              case value => throw Exception(s"Not supported going Up : ${value} ")
            warehouse((nextX, nextY)) = BoxS
            warehouse((nextX + 1, nextY)) = BoxE
            nextS.previous(direction).markBigBoxEmpty
          case Left =>
            warehouse(nextX + 1, nextY) match
              case Empty => ()
              case BoxE => nextS.moveBigBox(direction, BoxS)
              case value => throw Exception(s"Not supported : ${value}")
            warehouse((x - 1, y)) = BoxS
            warehouse((x, y)) = BoxE
            previous(direction).markBigBoxEmpty
          case Right =>
            warehouse(nextX, nextY) match
              case Empty => ()
              case BoxS => nextS.moveBigBox(direction, BoxS)
              case _ => throw Exception("Not supported")
            warehouse((x + 1, y)) = BoxS
            warehouse((x + 2, y)) = BoxE
            previous(direction).markBigBoxEmpty

        warehouse

enum Location:
  case Wall
  case Box
  case Empty
  case Robot
  case BoxS
  case BoxE

export Location._

object Location:
  def from(char: Char): Location =
    char match
      case '#' => Wall
      case 'O' => Box
      case '.' => Empty
      case '@' => Robot

enum Directions:
  case Up
  case Down
  case Right
  case Left

export Directions._

object Directions:
  def fromChar(char: Char): Directions =
    char match
      case '^' => Up
      case 'v' => Down
      case '>' => Right
      case '<' => Left
      case _ => throw Exception("Not managed")