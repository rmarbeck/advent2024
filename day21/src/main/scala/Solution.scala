import scala.annotation.targetName

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val result1 = lineByLine(inputLines, 2)

    val result2 = lineByLine(inputLines, 25)

    (s"$result1", s"$result2")

def lineByLine(inputLines: Seq[String], nbRobots: Int): Long =
  inputLines.map(input =>
    input.init.toLong * compute(input, 0, nbRobots)
  )
  .sum

case class Pos(x: Int, y: Int):
  @targetName("minus")
  def -(other: Pos): Pos = Pos(x - other.x, y - other.y)
  @targetName("add")
  def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
  lazy val projX: Pos = Pos(x, 0)
  lazy val projY: Pos = Pos(0, y)

object Pos:
  def from(pos: (Int, Int)): Pos = Pos(pos._1, pos._2)

val cache = collection.mutable.Map.empty[(Pos, Pos, Int, Int), Long]
def computeCost(from: Pos, dest:Pos, currentLevel: Int, nbRobots: Int): Long =
  cache.getOrElseUpdate((from, dest, currentLevel, nbRobots), {
    val positions = if currentLevel == 0 then NumericKey.valuesSet else DirectionalKey.valuesSet
    val diff = dest - from
    val horiz = (if diff.x > 0 then ">" else "<") * diff.x.abs
    val vert = (if diff.y > 0 then "v" else "^") * diff.y.abs
    val shouldVertBefore = !positions(from + diff.projX) || (positions(from + diff.projY) && diff.x > 0)
    val solution = if shouldVertBefore then vert + horiz + 'A' else horiz + vert + 'A'
    if currentLevel == nbRobots then solution.length else compute(solution, currentLevel + 1, nbRobots)
  })

def compute(goal: String, currentLevel: Int, nbRobots: Int): Long =
  val mapper = if currentLevel == 0 then NumericKey.asMap else DirectionalKey.asMap
  s"A$goal".map(mapper).sliding(2).map:
    case Seq(src, dst) => computeCost(src, dst, currentLevel, nbRobots)
  .sum

enum NumericKey(val position: (Int, Int), val char: Char):
  case A extends NumericKey((2, 3), 'A')
  case Zero extends NumericKey((1, 3), '0')
  case One extends NumericKey((0, 2), '1')
  case Two extends NumericKey((1, 2), '2')
  case Three extends NumericKey((2, 2), '3')
  case Four extends NumericKey((0, 1), '4')
  case Five extends NumericKey((1, 1), '5')
  case Six extends NumericKey((2, 1), '6')
  case Seven extends NumericKey((0, 0), '7')
  case Height extends NumericKey((1, 0), '8')
  case Nine extends NumericKey((2, 0), '9')

object NumericKey:
  lazy val valuesSet: Set[Pos] = asMap.values.toSet

  lazy val asMap: Map[Char, Pos] = NumericKey.values.map(k => k.char -> Pos.from(k.position)).toMap

enum DirectionalKey(val position: (Int, Int), val char: Char):
  case A extends DirectionalKey((2, 0), 'A')
  case Up extends DirectionalKey((1, 0), '^')
  case Left extends DirectionalKey((0, 1), '<')
  case Down extends DirectionalKey((1, 1), 'v')
  case Right extends DirectionalKey((2, 1), '>')

object DirectionalKey:
  lazy val valuesSet: Set[Pos] = asMap.values.toSet

  lazy val asMap: Map[Char, Pos] = DirectionalKey.values.map(k => k.char -> Pos.from(k.position)).toMap