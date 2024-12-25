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
          case "AND" => AND(Set(input1, input2), output)
          case "OR" => OR(Set(input1, input2), output)
          case _ => XOR(Set(input1, input2), output)

    given Rules = rules

    val result1 = connect(summon[Wires].keys.toList, Set(), summon[Wires])


    //rules.sortBy(_.index).foreach(println)
    {
      val result = swapRules("z07", "bjm")
      val result2 = swapRules("z13", "hsw")(using result)
      val result3 = swapRules("z18", "skf")(using result2)
      val result4 = swapRules("nvr", "wkr")(using result3)
      displayUntil(0, 44)(using result4)
    }



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

    val result2 = Set("z07", "bjm", "z13", "hsw", "z18", "skf", "nvr", "wkr").toList.sorted.mkString(",")


    (s"$result1", s"$result2")

def displayUntil(current: Int, max: Int, previous: Option[String] = None)(using rules: Rules): Unit =
  println(s"------------ => ${current}")
  println(rules.filter(_.index.exists(_ == current)))
  val currentRules = rules.filter(_.index.exists(_ == current))
  val others = currentRules.flatMap(_.values).distinct
  val otherRules = others.flatMap:
    oth => rules.filter(_.inputs.contains(oth))

  otherRules.distinct.foreach(println)
  current <= max match
    case false => ()
    case true =>
      current match
        case 0 =>
          println("case 0")
          displayUntil(current + 1, max, Some("gnn"))
        case _ =>

          val xor = rules.filter(_.index.exists(_ == current)).filterNot(_.output.startsWith("z")).collect:
            case rule: XOR => rule.output
          .head
          val and = rules.filter(_.index.exists(_ == current)).collect:
            case rule: AND => rule.output
          .head
          val xor2 = rules.filter(_.contains(xor, previous.get)).collect:
            case rule: AND => rule.output
          .headOption
          xor2 match
            case Some(value) =>
              val or = rules.filter(_.contains(value, and)).collect:
                case rule: OR => rule.output
              .head

              println(s"XOR is $xor, AND is $and, XOR2 is $value, OR is $or")
              println("------------")

              displayUntil(current + 1, max, Some(or))
            case None =>
              val pos1 = rules.filter(_.inputs.contains(xor))
              val pos2 = rules.filter(_.inputs.contains(previous.get))
              println(s"$pos1")
              println("OR")
              println(s"$pos2")
              println("IS NOT GOOD")






/*
def findErrors(currentIndex: Int, previous: Option[String], next: Option[String], rules: Rules, errors: List[String]): Int =
  val currentRules = rules.filter(_.index.forall(_ == currentIndex))
  currentIndex match
    case 0 =>
      val prev = currentRules.find(_.isInstanceOf[AND]).map(_.output)
      findErrors(1, None, prev, rules, errors)
    case 1 =>
      val newPrev = currentRules.filterNot(_.output.startsWith("z")).find(_.isInstanceOf[XOR]).map(_.output)
      val next = currentRules.filterNot(_.output.startsWith("z")).find(_.isInstanceOf[AND]).map(_.output)
      val currentRule = currentRules.filter(_.output.startsWith("z")).headOption
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
*/
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
    case rule: (AND | OR | XOR) if rule.output == output1 => rule.changeOutput(output2)
    case rule: (AND | OR | XOR) if rule.output == output2 => rule.changeOutput(output1)
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
            case Some(value) => Some(rule.output -> value)
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
            case Some(value) => Some(rule.output -> value)
            case None => None
      .toMap

      val newWiresToExplore = (tail ::: newWires.keys.toList).toSet -- explored

      connect(newWiresToExplore.toList, explored + head, in ++ newWires)


trait Rule:
  def values: Set[String] = Set(input1, input2, output)
  def changeOutput(newOutput: String): Rule
  def inputs: Set[String]
  lazy val input1: String = inputs.toList.sorted.head
  lazy val input2: String = inputs.toList.sorted.last
  def output: String

  def index: Option[Int] =
    (input1, input2, output) match
      case (s"x$index", _, _) => Some(index)
      case (_, s"y$index", _) => Some(index)
      case (_, _, s"z$index") => Some(index)
      case _ => None
  .map(_.toInt)

  def contains(first: String, second: String): Boolean =
    inputs.contains(first) && inputs.contains(second)

  def withInput(input: String): Boolean =
    input == input1 || input == input2
  def op: (Boolean, Boolean) => Boolean
  final def apply()(using wires: Wires): Option[Boolean] =
    for
      in1 <- wires.get(input1)
      in2 <- wires.get(input2)
    yield
      op(in1, in2)

case class AND(inputs: Set[String], output: String) extends Rule:
  override def changeOutput(newOutput: String) = this.copy(output = newOutput)
  override def op: (Boolean, Boolean) => Boolean = _ & _

case class OR(inputs: Set[String], output: String) extends Rule:
  override def changeOutput(newOutput: String) = this.copy(output = newOutput)
  override def op: (Boolean, Boolean) => Boolean = _ | _

case class XOR(inputs: Set[String], output: String) extends Rule:
  override def changeOutput(newOutput: String) = this.copy(output = newOutput)
  override def op: (Boolean, Boolean) => Boolean = _ ^ _

