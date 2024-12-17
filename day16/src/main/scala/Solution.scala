import Dir.{Down, Left, Right, Up}

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    import Dir._

     val (locations, start, end) = inputLines.zipWithIndex.foldLeft(Nil: List[Summit], (0, 0), (0, 0)):
        case (total, (line, y)) =>
          def addSummit(accumulator: List[Summit])(x: Int) = List(Up, Down, Right, Left).map(Summit(x, y, _)) ::: accumulator
          val partial = line.zipWithIndex.foldLeft((Nil, None, None): (List[Summit], Option[(Int, Int)], Option[(Int, Int)])):
            case (acc, ('.', x)) => (addSummit(acc._1)(x), acc._2, acc._3)
            case (acc, ('S', x)) => (addSummit(acc._1)(x), Some((x, y)), acc._3)
            case (acc, ('E', x)) => (addSummit(acc._1)(x), acc._2, Some((x, y)))
            case (acc, _) => acc
          (partial._1 ::: total._1, partial._2.getOrElse(total._2), partial._3.getOrElse(total._3))


    val (result1, result2) = Dijkstra.solve(asGraph(start, end, locations), Summit(start._1, start._2, Dir.Right), List(Up, Down, Right, Left).map(Summit(end._1, end._2, _)))

    (s"$result1", s"$result2")

enum Dir:
  case Up
  case Down
  case Right
  case Left

  def orthogonal: Seq[Dir] = this match
    case Up | Down => Seq(Right, Left)
    case Left | Right => Seq(Up, Down)


case class Summit(x: Int, y: Int, dir: Dir):
  import Dir._

  private def step: Summit =
    dir match
      case Up => this.copy(y = y - 1)
      case Down => this.copy(y = y + 1)
      case Left => this.copy(x = x - 1)
      case Right => this.copy(x = x + 1)

  def next: Seq[Summit] =
    this.step +: this.dir.orthogonal.map(newDir => this.copy(dir = newDir))

  def weightBetween(other: Summit): Long =
    (other.x - x, other.y - y) match
      case (0, 0) => 1000
      case _ => 1

  override def toString: String = s"$x,$y,$dir"

class GraphFromArray(val elements: Seq[Summit])(validNeighbours: Summit => Seq[Summit]) extends Graph[Summit]:
  override def getElements: Seq[Summit] = elements
  override def weightBetween(first: Data[Summit], second: Data[Summit]): Long =
    first.getElement.weightBetween(second.getElement)
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: Seq[Data[Summit]]): Seq[Data[Summit]] =
    val possibleNeighbours = validNeighbours.apply(current.getElement)
    potentialNeighbours.filter: summitWithData =>
      possibleNeighbours.contains(summitWithData.getElement)

private def asGraph(start: (Int, Int), end: (Int, Int), locations: Seq[Summit]): GraphFromArray =
  GraphFromArray(locations)((summit: Summit) => summit.next)