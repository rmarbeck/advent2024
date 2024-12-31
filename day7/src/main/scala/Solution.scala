import scala.annotation.targetName
import scala.collection.SeqView
import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable

type Operation = (Long, Long) => Long

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val equations = inputLines.collect:
      case s"${result}: ${terms}" => Equation(result, terms)

    val (matchingPart1, notMatchingPart1) = equations.par.partition(_.matchesPart1)

    val matchingPart2 = notMatchingPart1.par.withFilter(_.matchesPart2)

    val List(result1, result2Partial) =
      List(matchingPart1, matchingPart2).map(_.map(_.result).sum)

    val result2 = result1 + result2Partial

    (s"$result1", s"$result2")

case class Equation(result: Long, terms: List[Long]):
  import Equation.{allFunctions, functionsPart1}
  private def copies(ops: SeqView[Operation]): SeqView[Equation] =
    val (head, others) = (terms.head, terms.tail)
    ops.map:
      op => this.copy(terms = op(head, others.head) :: others.tail)

  private def matches(ops: SeqView[Operation]): Boolean =
    terms match
      case head :: Nil => head == result
      case head :: others => copies(ops).exists(_.matches(ops))
      case _ => false

  def matchesPart1: Boolean = matches(functionsPart1)

  def matchesPart2: Boolean = matches(allFunctions)


object Equation:
  private val allFunctions: SeqView[Operation] =
    ((_ + _) :: (_ * _) :: ((_ || _): Operation) :: Nil).view
  private val functionsPart1: SeqView[Operation] =
    allFunctions.toList.take(2).view
  def apply(resultStr: String, termsStr: String): Equation = Equation(resultStr.toLong, termsStr.split(" ").toList.map(_.toLong))


extension (current: Long)
  @targetName("concat")
  def ||(other: Long): Long = s"$current$other".toLong