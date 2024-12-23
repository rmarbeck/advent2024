import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

type Links = Map[String, Set[String]]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val grouped: Links = inputLines.flatMap:
      case s"$first-$second" => List(first -> second, second -> first)
    .groupMapReduce(_._1)((_, other) => Set(other))(_ ++ _)

    given Links = grouped

    val result = grouped.filter(_._1.startsWith("t")).flatMap((key, values) => connected(values.toList, Nil).map((f, s) => alpha(key, f, s))).toList.distinct

    import Order.given


    println(grouped.map((key, values) => values + key).map(_.toList))
    val resultb = findBest(TreeSet(grouped.map((key, values) => values + key).map(_.toList).toList:_ *))

    val result1 = s"${result}"
    val result2 = s"$resultb"

    (s"$result1", s"$result2")

object Order:
  given orderingList: Ordering[List[String]] = Ordering.by(str => (-str.length, str.sorted.mkString))

def alpha(one: String, two: String, three: String): String =
  List(one, two, three).sorted.mkString("-")

@tailrec
def findBest(toExplore: TreeSet[List[String]])(using links: Links): Int =
  toExplore match
    case empty if empty.isEmpty => 0
    case notEmpty =>
      val (head, tail) = (notEmpty.head, notEmpty.tail)
      val matches = head.forall:
        member =>
          val next = links(member) + member
          head.forall(next.contains)
      matches match
        case true => head.size
        case false => findBest(tail ++ head.combinations(head.size - 1))

@tailrec
def connected(list: List[String], current: List[(String, String)])(using links: Links): List[(String, String)] =
  list match
    case Nil => current
    case head :: tail =>
      val updatedList = tail.collect:
        case currentComputerInTail if links(currentComputerInTail).contains(head) => (currentComputerInTail, head)
      connected(tail, updatedList ::: current)

def best(setOfComputers: Set[String])(using links: Links): List[String] =
  setOfComputers match
    case empty if empty.isEmpty => Nil
    case notEmpty =>
      val (head, tail, currentSize) = (notEmpty.head, notEmpty.tail, notEmpty.size)
      tail.foldLeft(links(head) + head):
        case (acc, currentComputer) => acc intersect (links(currentComputer) + currentComputer)
      match
        case value if value.size == currentSize => value.toList
        case other =>
          setOfComputers.toList.combinations(currentSize - 1).map(subList => best(subList.toSet)).maxBy(_.length)



def findSetConnected(computer: String)(using links: Links): Int =
  val attached = links(computer)
  attached.map(cu => (links(cu) intersect(attached + computer)).size).max