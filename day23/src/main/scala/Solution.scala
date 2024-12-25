import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

type Links = Map[String, Set[String]]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val links: Links = inputLines.flatMap:
      case s"$first-$second" => List(first -> second, second -> first)
    .groupMapReduce(_._1)((_, other) => Set(other))(_ ++ _)

    given Links = links

    val result1 = links.filter(_._1.startsWith("t")).flatMap:
      (key, values) => connected(values, Set()).map((f, s) => alpha(key, f, s))
    .toList.distinct

    import Order.given

    val result2 = findBest(TreeSet(links.map((key, values) => values + key).map(_.toList).toList:_ *))

    (s"${result1.size}", s"$result2")

object Order:
  given orderingList: Ordering[List[String]] = Ordering.by(str => (-str.length, str.sorted.mkString))

def alpha(one: String, two: String, three: String): String =
  List(one, two, three).sorted.mkString("-")

@tailrec
def findBest(toExplore: TreeSet[List[String]])(using links: Links): String =
  def isTheOne(toTest: Set[String]): Boolean =
    toTest.forall:
      member =>
        val next = links(member) + member
        (toTest intersect next) == toTest
  toExplore match
    case empty if empty.isEmpty => ""
    case notEmpty =>
      val head = notEmpty.head
      if isTheOne(head.toSet) then
        head.sorted.mkString(",")
      else
        findBest(notEmpty.tail ++ head.combinations(head.size - 1))

@tailrec
def connected(input: Set[String], current: Set[(String, String)])(using links: Links): Set[(String, String)] =
  input match
    case empty if empty.isEmpty => current
    case notEmpty =>
      val (head, tail) = (notEmpty.head, notEmpty.tail)
      val updatedList = tail.collect:
        case currentComputerInTail if links(currentComputerInTail).contains(head) => (currentComputerInTail, head)
      connected(tail, updatedList ++ current)
