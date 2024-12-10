import scala.annotation.{tailrec, targetName}

type TopographicMap = Vector[Vector[Int]]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given grid: TopographicMap = inputLines.map(line => line.map(_.asDigit)).map(_.toVector).toVector

    val zeros =
      for
        y <- grid.indices; x <- grid(0).indices
        if grid(x)(y) == 0
      yield
        Position(x, y)

    val hikingTrails = zeros.map(findPaths)

    val result1 = s"${hikingTrails.map(_.distinct.length).sum}"
    val result2 = s"${hikingTrails.map(_.length).sum}"

    (s"$result1", s"$result2")

def findPaths(init: Position)(using grid: TopographicMap): List[Position] =
  @tailrec
  def find(positions: List[Position], currentLevel: Int = 1): List[Position] =
    currentLevel match
      case 10 => positions
      case _ =>
        find(positions.flatMap(_.aroundEquals(currentLevel)), currentLevel + 1)

  find(init :: Nil)

case class Position(x: Int, y: Int):
  private def isDefined(using grid: TopographicMap): Boolean = grid.isDefinedAt(x) && grid(x).isDefinedAt(y)
  @targetName("add")
  private def +(other: Position): Position = Position(x + other.x, y + other.y)
  private def value(using grid: TopographicMap): Int = grid(x)(y)

  def aroundEquals(value: Int)(using grid: TopographicMap): List[Position] = around.filter(_._2 == value).map(_._1)

  private def around(using grid: TopographicMap): List[(Position, Int)] =
    List((1 ,0), (0, -1), (-1 ,0), (0, 1)).map(Position.apply).collect:
      case drift if (this + drift).isDefined => (this + drift, (this + drift).value)

