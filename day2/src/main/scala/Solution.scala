import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val reports = inputLines.map(reportParser)

    val List(result1, result2) = List(isSafePart1, isSafePart2).map(reports.count)

    (s"$result1", s"$result2")

end Solution

val separator = " "
val maxDiff = 3

def reportParser(input: String): List[Int] = input.split(separator).map(_.toInt).toList

def isSafePart1(report: List[Int]): Boolean =
  def differences(rawValues: List[Int]): List[Int] =
    rawValues.sliding(2).toList.collect:
      case head :: last :: Nil => last - head
  val diffed = differences(report)
  val (min, max) = (diffed.min, diffed.max)
  (min * max > 0) && min >= -maxDiff && max <= maxDiff

def isSafePart2(toTest: List[Int]): Boolean = isSafePart1(toTest) || doesBecomeSafeByRemovingOne(toTest)

private def doesBecomeSafeByRemovingOne(report: List[Int]): Boolean =
  val size = report.size
  @tailrec
  def removeAndTest(index: Int): Boolean =
    def remove: List[Int] =
      report.zipWithIndex.collect:
        case (value, current) if current != index => value
    isSafePart1(remove) match
      case true => true
      case false if index == size => false
      case false => removeAndTest(index + 1)

  removeAndTest(0)
