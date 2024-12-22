import DirectionalKey.*

import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given Cache = Cache()

    val result1 = inputLines.map(input =>
        //println(s"${input.take(3).toInt} ${guessMoves(input).length}")
        input.take(3).toInt * guessMoves(input).length)
    .sum


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


def guessDirectionalMoves(goal: String): String =
  def guessMoveBetween(from: Char, to: Char): String =
    val List(numKey1, numKey2) = List(from, to).map(DirectionalKey.fromChar)
    numKey1.moveTo(numKey2)

  val unoptimized = s"A$goal".sliding(2).collect:
    case group if group.length == 2 => s"${guessMoveBetween(group.head, group.last)}A"
  .mkString

  /*val optimized = unoptimized.split("A").map:
    case current => current.permutations.distinct.map(c => s"${c}A").map(guessMoveBetween).toList.minBy(_.length)
  .mkString*/

  unoptimized

class Cache:
  import scala.collection.mutable
  val cache2: mutable.Map[String, Int] = mutable.Map()
  def optimizedCached(goal: String) = cache2.getOrElseUpdate(goal, guessDirectionalMovesCached(goal).length)
  val cache: mutable.Map[String, String] = mutable.Map()
  def guessDirectionalMovesCached(goal: String) = cache.getOrElseUpdate(goal, guessDirectionalMoves(goal))

def guessMoves(goal: String)(using cache: Cache): String =
  def guessMoveBetween(from: Char, to: Char): String =
    val List(numKey1, numKey2) = List(from, to).map(NumericKey.fromChar)
    numKey1.moveToEnhanced(numKey2)

  val first = s"A$goal".sliding(2).collect:
    case group if group.length == 2 => s"${guessMoveBetween(group.head, group.last)}A"
  .mkString

  findOptimized(first, s"A$goal")


def guessMoves2(goal: String)(using cache: Cache): Int =
  def guessMoveBetween(from: Char, to: Char): String =
    val List(numKey1, numKey2) = List(from, to).map(NumericKey.fromChar)
    numKey1.moveToEnhanced(numKey2)

  val first = s"A$goal".sliding(2).collect:
    case group if group.length == 2 => s"${guessMoveBetween(group.head, group.last)}A"
  .mkString

  findOptimized3(first, s"A$goal")


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


def findOptimized(wordContainingA: String, initialGoal: String, nb: Int = 2)(using cache: Cache): String =
  nb match
    case 0 => wordContainingA
    case 2 =>
      wordContainingA.split("A").zip(initialGoal.toCharArray).map:
        case (subWords, destination) =>
          subWords.permutations.filter(_.isValidFrom(destination)).toList.map:
            currentPermutation =>
              findOptimized(cache.guessDirectionalMovesCached(s"${currentPermutation}A"), "", nb - 1)
          .minBy(_.length)
      .mkString
    case _ =>
      findOptimized(guessDirectionalMoves(wordContainingA), "", nb - 1)
      /*wordContainingA.split("A").map:
        subWords =>
          subWords.permutations.toList.map:
            currentPermutation =>
              findOptimized(cache.guessDirectionalMovesCached(s"${currentPermutation}A"), "", nb - 1)
          .minBy(_.length)
      .mkString*/

def findOptimized3(wordContainingA: String, initialGoal: String, nb: Int = 25)(using cache: Cache): Int =
  val temp = wordContainingA.split("A").zip(initialGoal.toCharArray).map:
    case (subWords, destination) =>
      subWords.permutations.filter(_.isValidFrom(destination)).toList.map:
        currentPermutation =>
          findOptimized(cache.guessDirectionalMovesCached(s"${currentPermutation}A"), "", nb - 1)
      .minBy(_.length)
  .mkString
  findOptimized2(s"${temp}A", 10)


def findOptimized2b(wordsContainingA: List[String], nb: Int = 10)(using cache: Cache): Int =
  nb match
    case 0 => wordsContainingA.map(_.length).sum
    case _ =>
      wordContainingA.split("A").map:
        word => findOptimized2(cache.guessDirectionalMovesCached(s"${word}A"), nb - 1)
      .sum

def findOptimized2(wordContainingA: String, nb: Int = 10)(using cache: Cache): Int =
  nb match
    case 0 => wordContainingA.length
    case _ =>
      wordContainingA.split("A").map:
        word => findOptimized2(cache.guessDirectionalMovesCached(s"${word}A"), nb - 1)
      .sum
