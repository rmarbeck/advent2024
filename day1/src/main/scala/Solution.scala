object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (list1, list2) = inputLines.collect:
      case s"$first   $second" => (first.toInt, second.toInt)
    .unzip

    val res1 = list1.sorted.zip(list2.sorted).map:
      case (first, second) => Math.abs(first - second)
    .sum

    val List(grouped1, grouped2) = List(list1, list2).map(_.groupMapReduce(identity)(_ => 1)(_ + _))

    val res2 = grouped1.map:
      case (value, nbIn1) => value * (nbIn1 * grouped2.getOrElse(value, 0))
    .sum

    val result1 = s"$res1"
    val result2 = s"$res2"

    (s"$result1", s"$result2")

end Solution
