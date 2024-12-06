import scala.annotation.tailrec

type RuleSet = Map[Int, List[Int]]
type Update = List[Int]

object Solution:

  
  def run(inputLines: Seq[String]): (String, String) =

    val (rules, updates) = inputLines.foldLeft((Map(), Nil): (RuleSet, List[Update])):
      case (acc, RuleExt(rule)) => (rule.addToRulSet(acc._1), acc._2)
      case (acc, UpdateExt(values*)) => (acc._1, values.toList :: acc._2)
      case (acc, _) => acc

    given RuleSet = rules

    val (validReports, invalidReports) = updates.partition(isValid)

    val invalidReportsSorted = invalidReports.map:
      update =>
        given Update = update
        update.sorted(using UpdateOrdering.updateOrd)

    val List(result1, result2) = List(validReports, invalidReportsSorted).map(_.map(middle).sum)

    (s"$result1", s"$result2")


def middle(inList: List[Int]): Int = inList(inList.length / 2)

def isValid(update: List[Int])(using rules: RuleSet): Boolean =
  @tailrec
  def check(before: List[Int], next: List[Int]): Boolean =
    next match
      case Nil => true
      case head :: tail =>
        rules.get(head) match
          case None => check(head :: before, tail)
          case Some(constraints) =>
            constraints intersect before match
              case Nil => check(head :: before, tail)
              case _ => false

  check(List(update.head), update.tail)

//Ordering for part 2
object UpdateOrdering:
  given updateOrd(using rules: RuleSet, update: Update): Ordering[Int] with
    override def compare(x: Int, y: Int): Int =
      def currentSituationInList: Int = (update.indexOf(x) - update.indexOf(y)).sign
      (rules.get(y), rules.get(x)) match
        case (Some(value), _) if value.contains(x) => -1
        case (_, Some(value)) if value.contains(y) => 1
        case _ => currentSituationInList

case class Rule(before: Int, after: Int):
  def addToRulSet(rules: RuleSet): RuleSet =
    rules.get(before) match
      case Some(previousFollowers) => rules.updated(before, after :: previousFollowers)
      case None => rules + (before -> List(after))

//Extractors
object RuleExt:
  def apply(rule: Rule): String = s"${rule.before}|${rule.after}"
  def unapply(input: String): Option[Rule] =
    input match
      case s"$before|$after" => Some(Rule(before.toInt, after.toInt))
      case _ => None

object UpdateExt:
  def apply(parts: Int*): String =
    parts.mkString(",")
  def unapplySeq(whole: String): Option[Seq[Int]] =
    whole match
      case "" => None
      case _ => Some(whole.split(",").map(_.toInt).toSeq)

