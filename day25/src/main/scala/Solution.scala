object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val keysAndLocks = inputLines.grouped(8).map(_.take(7)).toList.collect:
      lines =>
        val (head, meaningfull) = (lines.head, convert(lines.tail))
        head.contains("#") match
          case false => Key(meaningfull)
          case true => Lock(meaningfull)

    val keys = keysAndLocks.collect:
      case k: Key => k

    val locks = keysAndLocks.collect:
      case l: Lock => l

    val result1 = locks.map:
      case l => keys.count(_.fits(l))
    .sum

    val result2 = s""

    (s"$result1", s"$result2")

case class Key(cols: Seq[Int]):
  def fits(lock: Lock): Boolean = cols.zip(lock.cols).forall(_ + _ <= 6)

case class Lock(cols: Seq[Int])

def convert(lines: Seq[String]): Seq[Int] =
  val transposed = lines.map(_.toCharArray).toArray.transpose
  transposed.map(_.count(_ == '#')).toSeq
