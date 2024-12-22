import DirectionalKey.*

import scala.annotation.tailrec

type Occurences = Int


object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given Cache = Cache()

    val result1 =
      given Occurences = 2
      inputLines.map(input =>
        input.take(3).toLong * guessMoves(input)
      )
      .sum

    (2 to 25).foreach:
      current => given Occurences = current
      val result = inputLines.map(input =>
          input.take(3).toLong * guessMoves(input)
        )
      .sum
      println(s"$current\t => $result")


    val result2 = 0/*inputLines.map(input =>
        input.take(3).toInt * guessMoves2(input))
      .sum*/

    /*inputLines.foreach(input =>
      val result = guessMoves(input)
      val trace1 = trace(result.toCharArray.toList)

      println(trace(trace1.toCharArray.toList))
      println(trace1)
      println(result)
    )*/


    (s"$result1", s"$result2")


enum NumericKey(val position: (Int, Int)):
  case A extends NumericKey(2, 0)
  case Zero extends NumericKey(1, 0)
  case One extends NumericKey(0, 1)
  case Two extends NumericKey(1, 1)
  case Three extends NumericKey(2, 1)
  case Four extends NumericKey(0, 2)
  case Five extends NumericKey(1, 2)
  case Six extends NumericKey(2, 2)
  case Seven extends NumericKey(0, 3)
  case Height extends NumericKey(1, 3)
  case Nine extends NumericKey(2, 3)

  def moveTo(other: NumericKey): String =
    val (firstPart, goesDown) = other.position._2 - this.position._2 match
      case value if value >= 0 => ("^"*value, false)
      case value => ("v"*value.abs, true)
    val (secondPart, goesRight) = other.position._1 - this.position._1 match
      case value if value >= 0 => (">"*value, true)
      case value => ("<"*value.abs, false)
    if (goesRight)
      s"$secondPart$firstPart"
    else if (goesDown)
      s"$secondPart$firstPart"
    else
      s"$firstPart$secondPart"

  def moveToEnhanced(other: NumericKey): String =
    moveTo(other)

  def nbMovesTo(other: NumericKey): Int =
    (other.position._1 - this.position._1).abs + (other.position._2 - this.position._2).abs

object NumericKey:
  def fromChar(char: Char): NumericKey =
    char match
      case 'A' => A
      case value => NumericKey.fromOrdinal((value.toInt - '0'.toInt) + 1)
  def fromPosition(position: (Int, Int)): NumericKey =
    NumericKey.values.find(_.position == position).get

enum DirectionalKey(val position: (Int, Int)):
  case A extends DirectionalKey(2, 1)
  case Up extends DirectionalKey(1, 1)
  case Left extends DirectionalKey(0, 0)
  case Down extends DirectionalKey(1, 0)
  case Right extends DirectionalKey(2, 0)

  def moveTo(other: DirectionalKey): String =
    val (firstPart, goesDown) = other.position._2 - this.position._2 match
      case value if value > 0 => ("^" * value, false)
      case value => ("v" * value.abs, true)
    val secondPart = other.position._1 - this.position._1 match
      case value if value >= 0 => ">" * value
      case value => "<" * value.abs

    if (goesDown)
      s"$firstPart$secondPart"
    else
      s"$secondPart$firstPart"


object DirectionalKey:
  def fromChar(char: Char): DirectionalKey =
    char match
      case 'A' => A
      case '^' => Up
      case 'v' => Down
      case '>' => Right
      case '<' => Left
  def fromPosition(position: (Int, Int)): DirectionalKey =
    DirectionalKey.values.find(_.position == position).get

extension (tuple: (Int, Int))
  def +(char: Char): (Int, Int) =
    DirectionalKey.fromChar(char) match
      case Up => (tuple._1, tuple._2 + 1)
      case Down => (tuple._1, tuple._2 - 1)
      case Left => (tuple._1 - 1, tuple._2 )
      case Right => (tuple._1 + 1, tuple._2)
      case _ => throw Exception("Not supported")

  def asChar: Char =
    tuple match
      case (2, 1) => 'A'
      case (1, 1) => '^'
      case (0, 0) => '<'
      case (1, 0) => 'v'
      case (2, 0) => '>'


def guessDirectionalMovesStd(goal: String): String =
  def guessMoveBetween(from: Char, to: Char): String =
    val List(numKey1, numKey2) = List(from, to).map(DirectionalKey.fromChar)
    numKey1.moveTo(numKey2)

  val unoptimized = s"A$goal".sliding(2).collect:
    case group if group.length == 2 => s"${guessMoveBetween(group.head, group.last)}A"
  .mkString

  unoptimized

def guessDirectionalMoves(goal: String): List[String] =
  def guessMoveBetween(from: Char, to: Char): String =
    val List(numKey1, numKey2) = List(from, to).map(DirectionalKey.fromChar)
    numKey1.moveTo(numKey2)

  val unoptimized = s"A$goal".sliding(2).collect:
    case group if group.length == 2 => s"${guessMoveBetween(group.head, group.last)}A"


  /*val optimized = unoptimized.split("A").map:
    case current => current.permutations.distinct.map(c => s"${c}A").map(guessMoveBetween).toList.minBy(_.length)
  .mkString*/

  unoptimized.toList

class Cache:
  import scala.collection.mutable
  given Cache = this
  val cache3: mutable.Map[(String, Int), Long] = mutable.Map()
  def optimizedCached2(goal: String, level: Int) = cache3.getOrElseUpdate((goal, level), findOptimizedSub3(goal, level))
  val cache2: mutable.Map[String, Int] = mutable.Map()
  def optimizedCached(goal: String) = cache2.getOrElseUpdate(goal, guessDirectionalMovesCached(goal).length)
  val cache: mutable.Map[String, List[String]] = mutable.Map()
  def guessDirectionalMovesCached(goal: String) = cache.getOrElseUpdate(goal, guessDirectionalMoves(goal))

def guessMoves(goal: String)(using cache: Cache, occurences: Occurences): Long =
  def guessMoveBetween(from: Char, to: Char): String =
    val List(numKey1, numKey2) = List(from, to).map(NumericKey.fromChar)
    numKey1.moveToEnhanced(numKey2)

  val first = s"A$goal".sliding(2).collect:
    case group if group.length == 2 => s"${guessMoveBetween(group.head, group.last)}A"
  .mkString

  findOptimized(first, s"A$goal")


def guessMoves2(goal: String)(using cache: Cache, occurences: Occurences): Long =
  def guessMoveBetween(from: Char, to: Char): String =
    val List(numKey1, numKey2) = List(from, to).map(NumericKey.fromChar)
    numKey1.moveToEnhanced(numKey2)

  val first = s"A$goal".sliding(2).collect:
    case group if group.length == 2 => s"${guessMoveBetween(group.head, group.last)}A"
  .mkString

  findOptimized(first, s"A$goal")


@tailrec
def trace(moves: List[Char], level1: (Int, Int) = (2, 1), output: List[Char] = Nil): String =
  moves match
    case Nil => output.mkString.reverse
    case 'A' :: tail =>
      trace(tail, level1, level1.asChar :: output)
    case other :: tail =>
      trace(tail, level1 + other, output)


extension (moves: String)
  def isValidFrom(c: Char): Boolean =
    c match
      case 'A' if moves.startsWith("<<") => false
      case '0' if moves.startsWith("<") => false
      case '1' if moves.startsWith("v") => false
      case '4' if moves.startsWith("vv") => false
      case '7' if moves.startsWith("vvv") => false
      case _ => true


def findOptimized(wordContainingA: String, initialGoal: String)(using cache: Cache, occurences: Occurences): Long =
  val firstLevel = wordContainingA.split("A").zip(initialGoal.toCharArray).map:
    case (subWords, destination) =>
      subWords.permutations.filter(_.isValidFrom(destination)).toList.map:
        currentPermutation =>
          findOptimizedPart1(guessDirectionalMovesStd(s"${currentPermutation}A"), occurences - 1)
      .min
  .sum
  firstLevel
  //findOptimizedPart1(guessDirectionalMoves(wordContainingA).mkString, nb - 1).length

def findOptimizedPart1(wordContainingA: String, nb: Int)(using cache: Cache): Long =
  nb match
    case 0 => wordContainingA.length
    case _ =>
      //val next = cache.guessDirectionalMovesCached(wordContainingA).mkString("A") + "A"
      import scala.collection.parallel._
      import collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
      findOptimizedSub3(wordContainingA, nb + 1)
      /*cache.guessDirectionalMovesCached(wordContainingA).par.map:
        sub => findOptimizedPart1(s"${sub}A", nb - 1)
      .sum*/
      //println(s"guessDirectionalMovesStd => ${guessDirectionalMovesStd(wordContainingA)}")
      //println(s"guessDirectionalMoves => ${guessDirectionalMoves(wordContainingA).mkString("A") + "A"}")
      //findOptimizedPart1(next, nb -1)


def findOptimizedSub(wordsContainingA: List[String], nb: Int)(using cache: Cache): Long =
  import scala.collection.parallel._
  import collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
  nb match
    case 0 =>
      wordsContainingA.map(_.length).sum
    case currentLevel =>
      wordsContainingA.par.map(current => findOptimizedSub(cache.guessDirectionalMovesCached(current), nb - 1)).sum

def findOptimizedSub3(wordContainingA: String, nb: Int)(using cache: Cache): Long =
  nb match
    case 0 => wordContainingA.length
    case other =>
      cache.guessDirectionalMovesCached(wordContainingA).map(c => cache.optimizedCached2(s"${c}A", nb - 1)).sum

def findOptimizedSub2(wordsContainingA: List[String], nb: Int, value: Long = 0L)(using cache: Cache): Long =
  nb match
    case 0 =>
      wordsContainingA.map(_.length).sum + value
    case currentLevel =>
      wordsContainingA match
        case Nil => value
        case head :: tail =>
          val subValue = findOptimizedSub2(cache.guessDirectionalMovesCached(head), nb - 1, value)
          findOptimizedSub2(tail, nb , subValue)
