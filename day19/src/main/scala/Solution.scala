import scala.annotation.tailrec

type Towels = Seq[Towel]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given  Towels = inputLines.head.split(",").map(_.trim).map(Towel.apply).toSeq

    val designs = inputLines.drop(2)

    val result1 = designs.filter(d =>isValid(List(d)))

    val result2 = result1.map(d =>ways2(List((d, Nil)))).map(_.length).sum

    result1.map(d => ways2(List((d, Nil)))).foreach:
      case ways =>
        println(s"${ways.head.reverse.mkString} -> ${ways.length}")
        println(s"\t${ways.map(_.reverse.mkString(",")).mkString(" - ")}");


    (s"${result1.length}", s"$result2")

case class Towel(stripes: String):
  override def toString: String = stripes

@tailrec
def isValid(remaining: Seq[String], explored: Seq[String] = Nil)(using towels: Towels): Boolean =
  remaining match
    case Nil => false
    case head :: tail =>
      towels.exists(_.stripes == head) match
        case true => true
        case false =>
          val remainingToAppend = towels.map(_.stripes).filter(head.startsWith).map(t => head.drop(t.length)).filterNot(explored.contains)
          isValid(remainingToAppend ++: remaining.tail, head +: explored)

@tailrec
def ways2(remaining: Seq[(String, List[Towel])], explored: Seq[(String, List[Towel])] = Nil, found: Seq[List[Towel]] = Nil)(using towels: Towels): Seq[List[Towel]] =
  remaining match
    case Nil => found
    case (current @ ("", towelsUpToNow)) :: tail =>
      ways2(remaining.tail, current +: explored, towelsUpToNow +: found)
    case (current @ (head, towelsUpToNow)) :: tail =>
      val remainingToAppend =
        towels.filter(t => head.startsWith(t.stripes)).map(t => (head.drop(t.stripes.length), t :: towelsUpToNow)).filterNot(explored.contains)

      ways2(remainingToAppend ++: remaining.tail, current +: explored, found)

@tailrec
def ways(remaining: Seq[(String, List[Towel])], explored: Seq[String] = Nil, found: Seq[List[Towel]] = Nil)(using towels: Towels): Seq[List[Towel]] =
  remaining match
    case Nil => found
    case (head, towelsUpToNow) :: tail =>
      towels.find(_.stripes == head) match
        case Some(towel) =>
          val remainingToAppend =
            towels.filterNot(_ == towel).filter(t => head.startsWith(t.stripes)).map(t => (head.drop(t.stripes.length), t :: towelsUpToNow)).filterNot((currentString, _) => explored.contains(currentString))

          ways(remainingToAppend ++: remaining.tail, explored, (towel :: towelsUpToNow) +: found)
        case None =>
          val remainingToAppend =
            towels.filter(t => head.startsWith(t.stripes)).map(t => (head.drop(t.stripes.length), t :: towelsUpToNow)).filterNot((currentString, _) =>explored.contains(currentString))

          ways(remainingToAppend ++: remaining.tail, head +: explored, found)