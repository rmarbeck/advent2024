import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

type Links = Map[String, Set[String]]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val grouped: Links = inputLines.flatMap:
      case s"$first-$second" => List(first -> second, second -> first)
    .groupMapReduce(_._1)((_, other) => Set(other))(_ ++ _)

    given Links = grouped

    val result1 = grouped.filter(_._1.startsWith("t")).flatMap((key, values) => connected(values.toList, Nil).map((f, s) => alpha(key, f, s))).toList.distinct

    import Order.given

    val result2 = findBest(TreeSet(grouped.map((key, values) => values + key).map(_.toList).toList:_ *))

    (s"${result1.size}", s"$result2")

object Order:
  given orderingList: Ordering[List[String]] = Ordering.by(str => (-str.length, str.sorted.mkString))

def alpha(one: String, two: String, three: String): String =
  List(one, two, three).sorted.mkString("-")

@tailrec
def findBest(toExplore: TreeSet[List[String]])(using links: Links): String =
  toExplore match
    case empty if empty.isEmpty => ""
    case notEmpty =>
      val (head, tail) = (notEmpty.head, notEmpty.tail)
      val matches = head.forall:
        member =>
          val next = links(member) + member
          head.forall(next.contains)
      matches match
        case true => head.sorted.mkString(",")
        case false => findBest(tail ++ head.combinations(head.size - 1))

@tailrec
def connected(list: List[String], current: List[(String, String)])(using links: Links): List[(String, String)] =
  list match
    case Nil => current
    case head :: tail =>
      val updatedList = tail.collect:
        case currentComputerInTail if links(currentComputerInTail).contains(head) => (currentComputerInTail, head)
      connected(tail, updatedList ::: current)
