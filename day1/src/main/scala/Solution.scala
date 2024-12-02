object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (left, right) = inputLines.collect:
      case s"$first   $second" => (first.toInt, second.toInt)
    .unzip

    val part1Result = left.sorted.zip(right.sorted).map:
      case (fromLeft, fromRight) => Math.abs(fromLeft - fromRight)
    .sum

    val List(leftOccurrences, rightOccurrences) = List(left, right).map(_.groupMapReduce(identity)(_ => 1)(_ + _))

    val part2Result = leftOccurrences.map:
      case (value, nbInLeft) => value * (nbInLeft * rightOccurrences.getOrElse(value, 0))
    .sum

    val result1 = s"$part1Result"
    val result2 = s"$part2Result"

    (s"$result1", s"$result2")

end Solution
