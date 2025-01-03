import OP._

import scala.annotation.tailrec

type Wires = Map[String, Boolean]
type Rules = Seq[Gate]
type BinOp = (Boolean, Boolean) => Boolean

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given wires: Wires = inputLines.collect:
      case s"${wire}: ${BoolExtr(state)}" => wire -> state
    .toMap

    given rules: Rules = inputLines.collect:
      case s"${input1} ${OPExtr(op)} ${input2} -> ${output}" => Gate(Set(input1, input2), output, op)

    val result1 = connect(wires.keys.toList, Set(), wires)

    val lastIndex = rules.flatMap(_.index).max - 1

    val result2 = rules.size match
      case 3 => "N/A"
      case _ =>
        val res = findErrors(0, lastIndex, None, rules, Nil)
        res.flatMap(_.toList).sorted.mkString(",")

    (s"$result1", s"$result2")

def findError(current: Int, previous: String, rules: Rules): (Option[String], Option[(String, String)], Rules) =
  val (one, two, three, four) = rules.foldLeft((None, None, None, None): (Option[Gate], Option[Gate], Option[Gate], Option[Gate])):
    case (acc, r@ Gate(IndexInputExtr(index), XOR)) if index == current => (Some(r), acc._2, acc._3, acc._4)
    case (acc, r@ Gate(IndexInputExtr(index), AND)) if index == current => (acc._1, Some(r), acc._3, acc._4)
    case (acc, r@ Gate(_, AND)) if r.contains(previous) => (acc._1, acc._2, Some(r), acc._4)
    case (acc, r@ Gate(_, XOR)) if r.contains(previous) => (acc._1, acc._2, acc._3, Some(r))
    case (acc, _) => acc

  val fiveOption = for
    t2 <- two; t3 <- three
  yield
    rules.collectFirst:
      case r@ Gate(_, OR) if r.inputs == Set(t2.output, t3.output) => r

  val swap1 = for
    o1 <- one; t2 <- two; t3 <- three
    if !t3.inputs.contains(o1.output)
  yield
    (o1.output, t3.inputs.filterNot(_ == previous).head)

  val swap4 = for
    f4 <- four
    if f4.output != s"z${current.asIndex}"
  yield
    (f4.output, s"z${current.asIndex}")

  (swap1, swap4, fiveOption) match
    case (swap@Some((first, second)), _, Some(five)) =>
      (five.map(_.output) , swap, swapRules(first, second)(using rules))
    case (_, swap@Some((first, second)), _) =>
      (four.map(_.output) ,swap, swapRules(first, second)(using rules))
    case _ => (fiveOption.get.map(_.output) ,None, rules)

@tailrec
def findErrors(current: Int, max: Int, previous: Option[String], rules: Rules, errors: List[(String, String)]): List[(String, String)] =
  current <= max match
    case false => errors
    case true =>
      current match
        case 0 =>
          val firstCarry = rules.collectFirst:
            case rule@ Gate(IndexInputExtr(0), AND) => rule
          findErrors(current + 1, max, firstCarry.map(_.output), rules, errors)
        case _ =>
          findError(current, previous.get, rules) match
            case (newPrevious, None, _) => findErrors(current + 1, max, newPrevious, rules, errors)
            case (newPrevious, error@Some(_), newRules) => findErrors(current, max, previous, newRules, error.get :: errors)


def swapRules(output1: String, output2: String)(using rules: Rules): Rules =
  rules.map:
    case rule if rule.output == output1 => rule.changeOutput(output2)
    case rule if rule.output == output2 => rule.changeOutput(output1)
    case rule => rule

@tailrec
def isOK(activeWiresToExplore: List[String], explored: Set[String], in: Wires)(using rules: Rules): Boolean =
  def asString(char: Char): String = in.filter(_._1.startsWith(s"$char")).toList.sortBy(_._1).map(_._2.asDigit).mkString.reverse
  activeWiresToExplore match
    case Nil =>
      asString('x') == asString('z').drop(1)
    case head :: tail =>
      given Wires = in
      val newWires = rules.filter(_.withInput(head)).flatMap:
        rule =>
          rule.apply() match
            case Some(value) => Some(rule.output -> value)
            case None => None
      .toMap

      val newWiresToExplore = (tail ::: newWires.keys.toList).toSet -- explored

      isOK(newWiresToExplore.toList, explored + head, in ++ newWires)

@tailrec
def connect(activeWiresToExplore: List[String], explored: Set[String], in: Wires)(using rules: Rules): Long =
  activeWiresToExplore match
    case Nil =>
      val asString = in.filter(_._1.startsWith("z")).toList.sortBy(_._1).map(_._2.asDigit).mkString
      asString.foldRight(0L):
        case (current, acc) => acc * 2 + current.asDigit
    case head :: tail =>
      given Wires = in
      val newWires = rules.filter(_.withInput(head)).flatMap:
        rule =>
          rule.apply() match
            case Some(value) => Some(rule.output -> value)
            case None => None
      .toMap

      val newWiresToExplore = (tail ::: newWires.keys.toList).toSet -- explored

      connect(newWiresToExplore.toList, explored + head, in ++ newWires)

enum OP(op: BinOp):
  case AND extends OP(_ & _)
  case OR extends OP(_ | _)
  case XOR extends OP(_ ^ _)

  def apply: BinOp = op.apply

case class IO private(input1: String, input2: String, output: String):
  def changeOutput(newOutput: String): IO = this.copy(output = newOutput)

  def inputs: Set[String] = Set(input1, input2)

  def index: Option[Int] =
    ((input1, input2, output) match
      case (s"x$index", _, _) => Some(index)
      case (_, s"y$index", _) => Some(index)
      case (_, _, s"z$index") => Some(index)
      case _ => None
      ).map(_.toInt)

  def contains(input: String): Boolean = inputs.contains(input)

  def contains(first: String, second: String): Boolean = contains(first) && contains(second)

  def withInput(input: String): Boolean = input == input1 || input == input2

object IO:
  def apply(inputs: Set[String], out: String): IO =
    require(inputs.size == 2)
    val Seq(first, second) = inputs.toSeq.sorted
    IO(first, second, out)

case class Gate(io: IO, op: OP):
  def changeOutput(newOutput: String): Gate = this.copy(io = io.changeOutput(newOutput))
  export io.{changeOutput as _, *}

  final def apply()(using wires: Wires): Option[Boolean] =
    for
      in1 <- wires.get(input1); in2 <- wires.get(input2)
    yield
      op.apply(in1, in2)

object Gate:
  def apply(inputs: Set[String], out: String, op: OP): Gate =
    Gate(IO.apply(inputs, out), op)

extension (index: Int)
  def asIndex:String = if index < 10 then s"0$index" else s"$index"

extension (bool: Boolean)
  def asDigit: String = if bool then "1" else "0"

//Extractors
object IndexInputExtr:
  def unapply(io: IO): Option[Int] =
    (
      (io.input1, io.input2) match
        case (s"x$index", _) => Some(index)
        case (_, s"y$index") => Some(index)
        case _ => None
      ).map(_.toInt)

object OPExtr:
  def unapply(asString: String): Option[OP] = Some(OP.valueOf(asString))

object BoolExtr:
  def unapply(bool: String): Option[Boolean] =
    bool match
      case "1" => Some(true)
      case "0" => Some(false)
      case _ => None