import scala.util.matching.Regex

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val inputAsOneLine = inputLines.mkString
    val inputCleaned = inputAsOneLine.replaceAll("""don\'t\(\)((?!do\(\)).)*""", "")

    val List(part1, part2) = List(inputAsOneLine, inputCleaned).map(multiply).map(_.sum)

    val result1 = s"$part1"
    val result2 = s"$part2"

    (s"$result1", s"$result2")

end Solution

val Mul: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r

def multiply(input: String): Iterator[Long] = for case Mul(first, second) <- Mul.findAllIn(input) yield first.toLong * second.toLong