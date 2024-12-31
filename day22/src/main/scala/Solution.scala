import scala.annotation.tailrec
import scala.collection.immutable
type Variation = (Int, Int, Int, Int)

val lastSecret: Int = 2000

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val secrets = inputLines.map(_.toLong)

    val result1 = secrets.map(nTh(lastSecret)).sum

    val (_ , result2) = secrets.view.flatMap(variationLists).groupMapReduce(_._1)(_._2)(_ + _).maxBy(_._2)

    (s"$result1", s"$result2")

def variationLists(secret: Long): Iterator[(Variation, Int)] =
  (1 to lastSecret).scanLeft(secret):
      case (acc, _) => next(acc)
    .map(secret => secret.toInt % 10)
  .sliding(5, 1).map(five => ((five(0) - five(1), five(1) - five(2), five(2) - five(3), five(3) - five(4)), five(4)))
  .distinctBy(_._1)

@tailrec
def nTh(nb: Int)(secret: Long): Long =
  nb match
    case 0 => secret
    case _ => nTh(nb - 1)(next(secret))

def next(secret: Long): Long =
  inline def prune(value: Long): Long = value % 16777216
  inline def mix(value: Long, in: Long): Long = value ^ in

  val secretB = prune(mix(secret * 64, secret))
  val secretC = prune(mix(secretB / 32, secretB))
  prune(mix(secretC * 2048, secretC))



