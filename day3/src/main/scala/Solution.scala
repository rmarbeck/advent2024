import scala.util.matching.Regex

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    val Mul: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
    def multiply(input: String) = for case Mul(first, second) <- Mul.findAllIn(input) yield first.toLong * second.toLong

    val inputAsOneLine = inputLines.mkString
    val inputCleaned = inputAsOneLine.replaceAll("""don't\(\)((?!do\(\)).)*""", "")

    val List(result1, result2) = List(inputAsOneLine, inputCleaned).map(multiply).map(_.sum)

    (s"$result1", s"$result2")