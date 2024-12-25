object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (keys, locks) =
      inputLines.grouped(8).map(_.take(7)).toSeq.collect:
        lines =>
          val (head, meaningfullLines) = (lines.head, convert(lines.tail))
          head.contains("#") match
            case false => Left(Key(meaningfullLines))
            case true => Right(Lock(meaningfullLines))
      .partitionMap(identity)

    val result1 = locks.map:
      l => keys.count(_.fits(l))
    .sum

    val result2 = s""

    (s"$result1", s"$result2")

case class Key(cols: Seq[Int]):
  def fits(lock: Lock): Boolean = cols.zip(lock.cols).forall(_ + _ <= 6)

case class Lock(cols: Seq[Int])

def convert(lines: Seq[String]): Seq[Int] =
  val transposed = lines.map(_.toCharArray).toArray.transpose
  transposed.map(_.count(_ == '#')).toSeq
