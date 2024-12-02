import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val reports = inputLines.map(reportParser)

    val resultPart1 = reports.count(isSafePart1)

    val resultPart2 = reports.count(isSafePart2)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"$result1", s"$result2")

end Solution

val separator = " "
val maxDiff = 3

def reportParser(input: String): List[Int] =
  input.split(separator).map(_.toInt).toList

def isSafePart1(report: List[Int]): Boolean =
  def differences(rawValues: List[Int]): List[Int] =
    rawValues.sliding(2, 1).map(list => list.last - list.head).toList
  val diffed = differences(report)
  val (min, max) = (diffed.min, diffed.max)
  (min * max > 0) && min >= -maxDiff && max <= maxDiff

def isSafePart2(toTest: List[Int]): Boolean =
  isSafePart1(toTest) || doesBecomeSafeByRemovingOne(toTest)

private def doesBecomeSafeByRemovingOne(report: List[Int]): Boolean =
  val size = report.size
  @tailrec
  def removeAndTest(index: Int): Boolean =
    def removeAt(index: Int): List[Int] =
      report.take(index - 1) ::: report.drop(index)
    isSafePart1(removeAt(index)) match
      case true => true
      case false if index == size => false
      case false => removeAndTest(index + 1)

  removeAndTest(1)
