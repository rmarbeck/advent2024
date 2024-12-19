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
  private lazy val length = stripes.length

  def dropBy(design: String): Option[String] = Option.when(design.startsWith(stripes))(design.drop(length))

def isValid(remaining: Seq[String])(using towels: Towels, cache: Cache): Boolean =
  remaining match
    case Nil => false
    case "" :: tail => true
    case head :: tail => towels.flatMap(_.dropBy(head)).exists(cache.isValidCached)

def successfullWays(subDesign: String)(using towels: Towels, cache: Cache): Long =
  subDesign match
    case "" => 1
    case _ => towels.flatMap(_.dropBy(subDesign)).filter(cache.isValidCached).map(cache.waysCached).sum

class Cache:
  given Cache = this
  import scala.collection.mutable
  private val isValidCache: mutable.Map[String, Boolean] = mutable.Map()
  private val nbOfWaysCache: mutable.Map[String, Long] = mutable.Map()

  def isValidCached(input: String)(using towels: Towels): Boolean =
    isValidCache.getOrElseUpdate(input, isValid(List(input)))

  def waysCached(input: String)(using towels: Towels): Long =
    nbOfWaysCache.getOrElseUpdate(input, successfullWays(input))