object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (list1, list2) = inputLines.foldLeft((Nil,Nil): (List[Int], List[Int])):
      case (acc, s"$first   $second") => (first.toInt :: acc._1, second.toInt :: acc._2)
      case _ => throw Exception("Input does not match rule")

    val res1 = list1.sorted.zip(list2.sorted).foldLeft(0):
      case (acc, (first, second)) => acc + Math.abs(first - second)

    val List(grouped1, grouped2) = List(list1, list2).map(_.groupMapReduce(identity)(_ => 1)(_ + _))

    val res2 = grouped1.map((value, nbIn1) => grouped2.get(value) match
      case Some(nbIn2) => value * (nbIn1 * nbIn2)
      case None => 0)
      .sum

    val result1 = s"$res1"
    val result2 = s"$res2"

    (s"$result1", s"$result2")

end Solution
