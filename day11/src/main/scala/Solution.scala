import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Map

type Stone = Long
type Frequency = Long

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val stones = inputLines.head.split(" ").map(_.toLong).toList

    val result1 = s"${calc2(stones, 25)}"
    val result2 = s"${calc2(stones, 75)}"

    (s"$result1", s"$result2")

extension (value: Long)
  def next: Seq[Stone] =
    value match
      case 0 => List(1)
      case value if value.toString.length % 2 ==0 =>
        val (left, right) = value.toString.splitAt(value.toString.length / 2)
        List(left.toLong, right.toLong)
      case other => List(value * 2024)

extension (stones: List[Stone])
  def toFrequencyList: List[(Stone, Frequency)] = stones.groupMapReduce(identity)(_ => 1L)(_ + _).toList

@tailrec
def calculate(stones: List[Stone], remainingSteps: Int): List[Stone] =
  remainingSteps match
    case 0 => stones
    case other => calculate(stones.flatMap(_.next), remainingSteps - 1)

object Cache:
  private val cache: mutable.Map[Stone, List[(Stone, Frequency)]] = mutable.Map()

  def calculateBy5Cached(stone: Stone): List[(Stone, Frequency)] =
    cache.getOrElseUpdate(stone, calculateBy5(stone))

def calculateBy5(stone: Stone): List[(Stone, Frequency)] =
  calculate(List(stone), 5).toFrequencyList

def calc2(stones: List[Stone], remainingSteps: Int): Long =
  calculate2(stones.toFrequencyList, remainingSteps).map(_._2).sum

@tailrec
def calculate2(stones: List[(Stone, Frequency)], remaingingSteps: Int): List[(Stone, Frequency)] =
  remaingingSteps match
    case 0 => stones
    case other =>
      val test = stones.flatMap:
        case (value, frequency) => (calculateBy5(value).map:
          case (stone, subFrequency) => (stone, subFrequency * frequency)
          )

      calculate2(test.groupMapReduce(_._1)(_._2)(_ + _).toList ,remaingingSteps - 5)
