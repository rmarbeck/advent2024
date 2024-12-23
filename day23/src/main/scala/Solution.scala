import scala.annotation.tailrec

type Links = Map[String, Set[String]]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val grouped: Links = inputLines.flatMap:
      case s"$first-$second" => List(first -> second, second -> first)
    .groupMapReduce(_._1)((_, other) => Set(other))(_ ++ _)

    given Links = grouped

    val result = grouped.filter(_._1.startsWith("t")).flatMap((key, values) => connected(values.toList, Nil).map((f, s) => alpha(key, f, s))).toList.distinct

    val resultb = grouped.keys.toList.foldLeft((0, grouped.keys)):
      case ((score, remaining), currentKey) =>
        val bestGroup = best(grouped(currentKey) + currentKey)
        (Math.max(score, bestGroup.length), remaining.filterNot(bestGroup.contains))


    val result1 = s"${result.length}"
    val result2 = s"$resultb"

    (s"$result1", s"$result2")

def alpha(one: String, two: String, three: String): String =
  List(one, two, three).sorted.mkString("-")

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