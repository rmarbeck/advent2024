import Button.*

import scala.annotation.targetName

type EquationBuilder = (Option[Rule], Option[Rule], Option[Prize])

object Solution:
  val aPressCost = 3
  val bPressCost = 1
  val part2PrizeAddition = 10_000_000_000_000L

  def run(inputLines: Seq[String]): (String, String) =

    val equationBuilders = inputLines.grouped(4).toList.collect:
      _.foldLeft((None, None, None): EquationBuilder):
          case (acc, s"Button A: X+${driftX}, Y+${driftY}") =>  (Some(Rule(A, driftX, driftY)), acc._2, acc._3)
          case (acc, s"Button B: X+${driftX}, Y+${driftY}") =>  (acc._1, Some(Rule(B, driftX, driftY)), acc._3)
          case (acc, s"Prize: X=${scoreX}, Y=${scoreY}")    =>  (acc._1, acc._2, Some(Prize(scoreX, scoreY)))
          case (acc, _) => acc

    val List(result1, result2) = List(Equation.fromBuilderPart1, Equation.fromBuilderPart2).map(equationBuilders.map(_).map(_.price).sum)

    (s"$result1", s"$result2")

case class Equation(ruleA: Rule, ruleB: Rule, prize: Prize):
  /*
   *    a * x + b * y = e
   *    c * x + d * y = f
   */
  val (a, b, c, d, e, f) = (ruleA.driftX, ruleB.driftX, ruleA.driftY, ruleB.driftY, prize.scoreX, prize.scoreY)
  private lazy val det = (a * d) - (b * c)
  private lazy val numX = (e * d) - (b * f)
  private lazy val numY = (a * f) - (e * c)
  def solve: Option[(Long, Long)] =
    det match
      case 0 => None
      case denominatorNotNull =>
        for
          x <- (numX / denominatorNotNull.toDouble).asLong; y <- (numY / denominatorNotNull.toDouble).asLong
        yield
          (x, y)

  def price: Long =
    solve match
      case None => 0
      case Some(x, y) => Solution.aPressCost * x + Solution.bPressCost * y

object Equation:
  def fromBuilderPart1(builder: EquationBuilder): Equation =
    (for
      ruleA <- builder._1; ruleB <- builder._2; prize <- builder._3
    yield
      Equation(ruleA, ruleB, prize)
    ) match
      case Some(equation) => equation
      case _ => throw Exception("Builder is not fully defined")

  def fromBuilderPart2(builder: EquationBuilder): Equation =
    fromBuilderPart1(builder.copy(_3 = builder._3.map(_ + Solution.part2PrizeAddition)))

enum Button:
  case A, B

case class Rule(button: Button, driftX: Long, driftY: Long)
object Rule:
  def apply(button: Button, driftX: String, driftY: String) = new Rule(button, driftX.toLong, driftY.toLong)

case class Prize(scoreX: Long, scoreY: Long):
  @targetName("add")
  def +(amount: Long): Prize = Prize(scoreX + amount, scoreY + amount)

object Prize:
  def apply(scoreX: String, scoreY: String) = new Prize(scoreX.toLong, scoreY.toLong)

extension (doubleValue: Double)
  def asLong: Option[Long] =
    doubleValue.toLong match
      case longValue if longValue == doubleValue => Some(longValue)
      case _ => None