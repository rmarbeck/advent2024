import scala.annotation.tailrec
import scala.collection.mutable

type Stone = Long
type Frequencies = List[(Stone, Long)]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val stones = inputLines.head.split(" ").map(_.toLong).toList

    val result1 = s"${blink(stones, 25, 5)}"
    val result2 = s"${blink(stones, 75, 5)}"

    (s"$result1", s"$result2")

def blink(stones: List[Stone], remainingSteps: Int, batchSize: Int = 5): Long =
  @tailrec
  def blinkRec(stones: Frequencies, remainingSteps: Int): Frequencies =
    remainingSteps match
      case 0 => stones
      case other =>
        val frequenciesAfterBatchSteps = stones.flatMap:
          case (value, frequency) => calculateByBatch(value, batchSize).map:
            case (stone, subFrequency) => (stone, subFrequency * frequency)

        blinkRec(frequenciesAfterBatchSteps.groupMapReduce(_._1)(_._2)(_ + _).toList, remainingSteps - batchSize)

  blinkRec(stones.toFrequencyList, remainingSteps).map(_._2).sum


object Cache:
  private val cache: mutable.Map[Stone, Frequencies] = mutable.Map()

  def calculateByBatchCached(stone: Stone, batchSize: Int): Frequencies =
    cache.getOrElseUpdate(stone, calculateByBatch(stone, batchSize))

def calculateByBatch(stone: Stone, batchSize: Int): Frequencies =
  @tailrec
  def calculateBy1(stones: List[Stone], remainingSteps: Int): List[Stone] =
    remainingSteps match
      case 0 => stones
      case other => calculateBy1(stones.flatMap(_.next), remainingSteps - 1)

  calculateBy1(List(stone), batchSize).toFrequencyList

extension (value: Long)
  def next: Seq[Stone] =
    value match
      case 0 => List(1)
      case value if value.toString.length % 2 ==0 =>
        val (left, right) = value.toString.splitAt(value.toString.length / 2)
        List(left.toLong, right.toLong)
      case other => List(value * 2024)

extension (stones: List[Stone])
  def toFrequencyList: Frequencies = stones.groupMapReduce(identity)(_ => 1L)(_ + _).toList
