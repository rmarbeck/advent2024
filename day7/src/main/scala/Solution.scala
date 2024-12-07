import scala.annotation.{tailrec, targetName}
import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable

type Operation = (Long, Long) => Long

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val equations = inputLines.collect:
      case s"${result}: ${terms}" => Equation(result, terms)

    val (matchingPart1, notMatchingPart1) = equations.par.partition(matching(Part1))

    val matchingPart2 = notMatchingPart1.par.withFilter(matching(Part2))

    val List(result1, result2Partial) =
      List(matchingPart1, matchingPart2).map(_.map(_.result).sum)

    val result2 = result1 + result2Partial

    (s"$result1", s"$result2")

case class Equation(result: Long, terms: List[Long])
object Equation:
  def apply(resultStr: String, termsStr: String): Equation = Equation(resultStr.toLong, termsStr.split(" ").toList.map(_.toLong))

def matching(part: Part)(eq: Equation): Boolean =
  val allFunctions: List[Operation] =
      (_ + _) :: (_ * _) :: ((_ || _): Operation) :: Nil

  val functionsToApply =
    part match
      case Part1 => allFunctions.take(2)
      case Part2 => allFunctions

  val Equation(result, terms) = eq
  val diffs = terms.scanRight(0L)(_ + _)
  val corrections = terms.length to 0 by -1
  val minimums = diffs.zip(corrections).map(_ - _ - result).tail

  @tailrec
  def doMatch(current: Set[Long], remainingTerms: List[Long], minimums: List[Long]): Boolean =
    remainingTerms match
      case head :: tail =>
        val next =
          functionsToApply
            .map(func =>
              current.map(func.apply(_ , head)).filter(_ + minimums.head <= 0))
            .fold(Set())(_ ++ _)
        if (next.isEmpty)
          false
        else
          doMatch(next, tail, minimums.tail)
      case Nil => current(result)

  doMatch(Set(terms.head), terms.tail, minimums.tail)

enum Part:
  case Part1; case Part2
export Part._

extension (current: Long)
  @targetName("concat")
  def ||(other: Long): Long = s"$current$other".toLong