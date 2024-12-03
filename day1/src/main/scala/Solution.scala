object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (left, right) = inputLines.collect:
      case s"$first   $second" => (first.toInt, second.toInt)
    .unzip

    val result1 = left.sorted.zip(right.sorted).map:
      case (fromLeft, fromRight) => Math.abs(fromLeft - fromRight)
    .sum

    val List(leftOccurrences, rightOccurrences) = List(left, right).map(_.groupMapReduce(identity)(_ => 1)(_ + _))

    val result2 = leftOccurrences.map:
      case (value, nbInLeft) => value * (nbInLeft * rightOccurrences.getOrElse(value, 0))
    .sum

    (s"$result1", s"$result2")