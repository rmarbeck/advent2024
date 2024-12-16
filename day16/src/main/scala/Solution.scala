type Locations = Seq[Location]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    import Dir._
    val locations =
      (inputLines.zipWithIndex.flatMap:
        case (line, y) => line.zipWithIndex.collect:
          case ('.', x) => List(Up, Down, Right, Left).map(Location(x, y, _, false, false))
          case ('S', x) => List(Up, Down, Right, Left).map(Location(x, y, _, true, false))
          case ('E', x) => List(Up, Down, Right, Left).map(Location(x, y, _, false, true))
        .flatten
        )

    println(locations.find(_.isEnd))
    println(locations.find(_.isStart))

    //
    // Code is here
    //



    val result1 = s""
    val result2 = s""

    (s"$result1", s"$result2")

enum Dir:
  case Up
  case Down
  case Right
  case Left

case class Location(x: Int, y: Int, dir: Dir, isStart: Boolean, isEnd: Boolean):
  import Dir._
  private def step: Location =
    dir match
      case Up => this.copy(y = y - 1)
      case Down => this.copy(y = y + 1)
      case Left => this.copy(x = x - 1)
      case Right => this.copy(x = x + 1)

  def next: Seq[Location] =
    this.step +: Dir.values.filterNot(_ == dir).map(newDir => this.copy(dir = newDir))
    
  def toSummit: Summit = Summit(x, y, dir)
  def weightBetween(other: Location): Long =
    (other.x - x, other.y - y,  dir, other.dir) match
      case (diffX, diffY, _, _) if diffX > 1 || diffY > 1 => throw Exception("Not supported")
      case (0, 0, Up | Down, Left | Right) => 1001
      case (0, 0, Left | Right, Up | Down) => 1001
      case (1 | -1, 0, Up | Down, Up | Down) => 1001
      case (1, 0, Right, Right) => 1
      case (-1, 0, Left, Left) => 1
      case (0, 1 | - 1, Right | Left, Right | Left) => 1001
      case (0, -1 , Up, Up) => 1
      case (0, 1, Down, Down) => 1
      case (1 | -1, 0, _, _) => throw Exception("Not supported")



