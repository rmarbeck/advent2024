import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val stones = inputLines.head.split(" ").map(_.toLong).map(Stone.apply).toList

    println(calculate(stones, 25).length)



    val result1 = s""
    val result2 = s""

    (s"$result1", s"$result2")

case class Stone(value: Long):
  def next: Seq[Stone] =
    value match
      case 0 => List(Stone(1))
      case value if value.toString.length % 2 ==0 =>
        val (left, right) = value.toString.splitAt(value.toString.length / 2)
        List(Stone(left.toLong), Stone(right.toLong))
      case other => List(Stone(value * 2024))

@tailrec
def calculate(stones: List[Stone], remaingingSteps: Int): List[Stone] =
  remaingingSteps match
    case 0 => stones
    case other => calculate(stones.flatMap(_.next), remaingingSteps - 1)