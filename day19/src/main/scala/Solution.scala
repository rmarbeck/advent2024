import scala.annotation.tailrec
import scala.collection.mutable

type Towels = Seq[Towel]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given Towels = inputLines.head.split(",").map(_.trim).map(Towel.apply).toSeq
    val designs = inputLines.drop(2)
    given cache: Cache = Cache()

    val result1 = designs.filter(cache.isValidCached)
    val result2 = result1.map(cache.waysCached).sum

    (s"${result1.length}", s"$result2")

case class Towel(stripes: String):
  lazy val length = stripes.length
  override def toString: String = stripes

  def dropBy(design: String): Option[String] =
    design.startsWith(stripes) match
      case true => Some(design.drop(length))
      case false => None


class Cache:
  given Cache = this
  private val isValidCache: mutable.Map[String, Boolean] = mutable.Map()
  def isValidCached(input: String)(using towels: Towels): Boolean =
    isValidCache.getOrElseUpdate(input, isValid(List(input)))

  private val nbOfWaysCache: mutable.Map[String, Long] = mutable.Map()
  def waysCached(input: String)(using towels: Towels): Long =
    nbOfWaysCache.getOrElseUpdate(input, successfullWays(input))

def isValid(remaining: Seq[String])(using towels: Towels, cache: Cache): Boolean =
  remaining match
    case Nil => false
    case head :: tail =>
      if towels.exists(_.stripes == head) then
        true
      else
        towels.flatMap(_.dropBy(head)).exists(cache.isValidCached)

def successfullWays(subDesign: String)(using towels: Towels, cache: Cache): Long =
  subDesign match
    case "" => 1
    case _ =>
      towels.flatMap(_.dropBy(subDesign)).filter(current => current.isBlank || cache.isValidCached(current)).map(cache.waysCached).sum
