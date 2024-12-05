import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (rules, updates) = inputLines.foldLeft((Map(), Nil): (Map[Int, List[Int]], List[List[Int]])):
      case (acc, s"$before|$after") =>
        acc._1.get(before.toInt) match
          case Some(existingList) => (acc._1 + (before.toInt -> (after.toInt :: existingList)), acc._2)
          case None => (acc._1 + (before.toInt -> (after.toInt :: Nil)), acc._2)
      case (acc, values) if values != "" => (acc._1, values.split(",").map(_.toInt).toList :: acc._2)
      case (acc, _) => acc

    println(updates.filter(isValid(_, rules)).map(middle).sum)

    //
    // Code is here
    //



    val result1 = s""
    val result2 = s""

    (s"$result1", s"$result2")

def middle(inList: List[Int]): Int = inList(inList.length / 2)

def isValid(update: List[Int], rules: Map[Int, List[Int]]): Boolean =
  @tailrec
  def check(before: List[Int], next: List[Int]): Boolean =
    next match
      case head :: tail =>
        rules.get(head) match
          case None => check(head :: before, tail)
          case Some(constraints) =>
            constraints intersect before match
              case head :: tail => false
              case Nil => check(head :: before, tail)
      case Nil => true

  check(List(update.head), update.tail)
