import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

type Links = Map[String, Set[String]]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val links: Links = inputLines.flatMap:
      case s"$first-$second" => List(first -> second, second -> first)
    .groupMapReduce(_._1)((key, other) => Set(key, other))(_ ++ _)

    given Links = links

    val result1 = links.filter(_._1.startsWith("t")).flatMap:
      (key, values) => connected(values - key).map(alpha(key, _, _))
    .toSet

    val result2 = findBest(TreeSet(links.map((key, values) => values + key).map(l => Computers.apply(l.toList)).toSeq:_ *))

    (s"${result1.size}", s"$result2")

def alpha(threes: String*): String =
  List(threes:_ *).sorted.mkString("-")

case class Computers private (names: Set[String], order: Int):
  override lazy val toString: String = names.toList.sorted.mkString(",")
  private lazy val length: Int = names.size
  lazy val combos: Iterator[Computers] = names.toList.combinations(length - 1).map(Computers.apply)
  def addComputer(name: String): Computers = Computers(names + name, order + name.hashCode)

object Computers:
  def apply(names: List[String]): Computers = Computers(names.toSet, names.hashCode())
  given orderingListOfString: Ordering[Computers] = Ordering.by(computers => (-computers.length, computers.order))

@tailrec
def findBest(toExplore: TreeSet[Computers])(using links: Links): String =
  def isTheOne(toTest: Set[String]): Boolean =
    toTest.forall:
      member => toTest.subsetOf(links(member))
  toExplore match
    case empty if empty.isEmpty => ""
    case notEmpty =>
      val head = notEmpty.head
      if isTheOne(head.names) then
        head.toString
      else
        findBest(notEmpty.tail ++ head.combos)

@tailrec
def connected(input: Set[String], current: Set[(String, String)] = Set())(using links: Links): Set[(String, String)] =
  input match
    case empty if empty.isEmpty => current
    case notEmpty =>
      val (head, tail) = (notEmpty.head, notEmpty.tail)
      val updatedList = tail.collect:
        case currentComputerInTail if links(currentComputerInTail).contains(head) => (currentComputerInTail, head)
      connected(tail, updatedList ++ current)
