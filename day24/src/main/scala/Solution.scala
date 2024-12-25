import scala.annotation.tailrec

type Wires = Map[String, Boolean]
type Rules = Seq[Rule]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given Wires = inputLines.collect:
      case s"${wire}: 1" => wire -> true
      case s"${wire}: 0" => wire -> false
    .toMap

    val rules = inputLines.collect:
      case s"${input1} ${op} ${input2} -> ${output}" =>
        op match
          case "AND" => AND(input1, input2, output)
          case "OR" => OR(input1, input2, output)
          case _ => XOR(input1, input2, output)

    given Rules = rules

    val result1 = connect(summon[Wires].keys.toList, Set(), summon[Wires])

    rules.sortBy(_.index).foreach(println)

    /*val badOnes =
      (0 to 44).filter:
        index =>
          given Wires = buildWires(index)
          !isOK(summon[Wires].keys.toList, Set(), summon[Wires])

    badOnes.foreach:
      index =>
        val head = rules.head
        given Wires = buildWires(index)
        val found = rules.tail.find:
          case (rule) =>
            val newRules = swapRules(rule.out, head.out)
            isOK(summon[Wires].keys.toList, Set(), summon[Wires])(using newRules)

        println(found)
    */

    val result2 = 2

    (s"$result1", s"$result2")

def findErrors(currentIndex: Int, previous: Option[String], next: Option[String], rules: Rules, errors: List[String]): Int =
  val currentRules = rules.filter(_.index.forall(_ == currentIndex))
  currentIndex match
    case 0 =>
      val prev = currentRules.find(_.isInstanceOf[AND]).map(_.out)
      findErrors(1, None, prev, rules, errors)
    case 1 =>
      val newPrev = currentRules.filterNot(_.out.startsWith("z")).find(_.isInstanceOf[XOR]).map(_.out)
      val next = currentRules.filterNot(_.out.startsWith("z")).find(_.isInstanceOf[AND]).map(_.out)
      val currentRule = currentRules.filter(_.out.startsWith("z")).headOption
      val test = (for
        np <- newPrev; ne <- next; cu <- currentRule
      yield
        cu.contains(np, ne)
        )
      test match
        case Some(true) => findErrors(currentIndex + 1, newPrev, next, rules, errors)
        case Some(false) =>
          findErrors(currentIndex + 1, newPrev, next, rules, errors)
        case _ => throw Exception("Not supported")

def buildWires(rank: Int): Wires =
  def padded(value: Int): String =
    value match
      case v if v <= 9 => s"0$v"
      case v => s"$v"

  (for
    x <- 0 to 44
  yield
    List(s"x${padded(x)}" -> (x == rank), s"y${padded(x)}" -> false)
  ).flatten.toMap


def swapRules(output1: String, output2: String)(using rules: Rules): Rules =
  rules.map:
    case rule1 @ AND(_, _, output) if output == output1 => rule1.copy(output = output2)
    case rule1 @ AND(_, _, output) if output == output2 => rule1.copy(output = output1)
    case rule1 @ OR(_, _, output) if output == output1 => rule1.copy(output = output2)
    case rule1 @ OR(_, _, output) if output == output2 => rule1.copy(output = output1)
    case rule1 @ XOR(_, _, output) if output == output1 => rule1.copy(output = output2)
    case rule1 @ XOR(_, _, output) if output == output2 => rule1.copy(output = output1)
    case rule => rule

extension (bool: Boolean)
  def asDigit: String =
    bool match
      case true => "1"
      case false => "0"

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
            case Some(value) => Some(rule.out -> value)
            case None => None
      .toMap

      val newWiresToExplore = (tail ::: newWires.keys.toList).toSet -- explored

      isOK(newWiresToExplore.toList, explored + head, in ++ newWires)



@tailrec
def connect(activeWiresToExplore: List[String], explored: Set[String], in: Wires)(using rules: Rules): Long =
  activeWiresToExplore match
    case Nil =>
      println(s" ${in.filter(_._1.startsWith("x")).toList.sortBy(_._1).map(_._2.asDigit).mkString.reverse}")
      println(s" ${in.filter(_._1.startsWith("y")).toList.sortBy(_._1).map(_._2.asDigit).mkString.reverse}")
      val asString = in.filter(_._1.startsWith("z")).toList.sortBy(_._1).map(_._2.asDigit).mkString
      println(asString.reverse)
      asString.foldRight(0L):
        case (current, acc) => acc * 2 + current.asDigit
    case head :: tail =>
      given Wires = in
      val newWires = rules.filter(_.withInput(head)).flatMap:
        rule =>
          rule.apply() match
            case Some(value) => Some(rule.out -> value)
            case None => None
      .toMap

      val newWiresToExplore = (tail ::: newWires.keys.toList).toSet -- explored

      connect(newWiresToExplore.toList, explored + head, in ++ newWires)


trait Rule(val in1: String, val in2: String, val out: String):
  def index: Option[Int] =
    (in1, in2, out) match
      case (s"x$index", _, _) => Some(index)
      case (s"y$index", _, _) => Some(index)
      case (s"z$index", _, _) => Some(index)
      case (_, s"x$index", _) => Some(index)
      case (_, s"y$index", _) => Some(index)
      case (_, s"z$index", _) => Some(index)
      case (_, _, s"z$index") => Some(index)
      case _ => None
  .map(_.toInt)

  def contains(first: String, second: String): Boolean =
    (in1 == first || in1 == second) && (in2 == first || in2 == second)

  def withInput(input: String): Boolean =
    input == in1 || input == in2
  def op: (Boolean, Boolean) => Boolean
  final def apply()(using wires: Wires): Option[Boolean] =
    for
      input1 <- wires.get(in1)
      input2 <- wires.get(in2)
    yield
      op(input1, input2)

case class AND(input1: String, input2: String, output: String) extends Rule(input1 ,input2, output):
  override def op: (Boolean, Boolean) => Boolean = _ & _

case class OR(input1: String, input2: String, output: String) extends Rule(input1 ,input2, output):
  override def op: (Boolean, Boolean) => Boolean = _ | _

case class XOR(input1: String, input2: String, output: String) extends Rule(input1 ,input2, output):
  override def op: (Boolean, Boolean) => Boolean = _ ^ _

