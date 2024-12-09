import scala.annotation.tailrec
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val blocks = inputLines.head.grouped(2).zipWithIndex.toArray.map:
      case (size, index) => Block(index, size.head.asDigit, size.last.asDigit)

    println(blocks.length)

    println(compute(blocks))

    val result1 = s""
    val result2 = s""

    (s"$result1", s"$result2")


case class Block(id: Int,fileLength: Int, free: Int):
  def splitAt(length: Int): (Block, Block) = (this.copy(fileLength = length, free = 0), this.copy(fileLength = fileLength - length, free = free))
  def sumAt(startIndex: Int): Long =
    require(fileLength > 0)
    val result1 = (id.toFloat * (2f * startIndex.toFloat + fileLength.toFloat -1f) * fileLength.toFloat / 2f).toLong
    val result2 =  (id.toDouble * (2d * startIndex.toDouble + fileLength.toDouble -1d) * fileLength.toDouble / 2d).toLong
    if (result2 != result1)
      println(s"$result2 != $result1")
      println(s"${id.toFloat} ${startIndex.toFloat} ${fileLength.toFloat} ${((2f * startIndex.toFloat + fileLength.toFloat -1f) * fileLength.toFloat / 2f).toLong}")
      println(s"${id.toDouble} ${startIndex.toDouble} ${fileLength.toDouble} ${((2d * startIndex.toDouble + fileLength.toDouble -1d) * fileLength.toDouble / 2d).toLong}")
    result2

  def freeIt: Block = this.copy(fileLength = 0)

def compute(blocks: Array[Block]): Long =
  @tailrec
  def computeRec(blocksDQ: mutable.ArrayDeque[Block], index: Int, value: Long): Long =
    blocksDQ.head match
      case Block(_, 0, freeInHead) =>
        blocksDQ.length match
          case 1 => value
          case _ => blocksDQ.last match
            case last @ Block(_, fileLengthInLast, _) if fileLengthInLast <= freeInHead =>
              blocksDQ.removeHead()
              blocksDQ.prepend(last.copy(free = freeInHead - fileLengthInLast))
              blocksDQ.removeLast()
              computeRec(blocksDQ, index, value)
            case last @ Block(_, fileLengthInLast, _) =>
              blocksDQ.removeHead()
              blocksDQ.removeLast()
              blocksDQ.append(last.copy(fileLength = fileLengthInLast - freeInHead))
              blocksDQ.prepend(last.copy(fileLength = freeInHead, free = 0))
              computeRec(blocksDQ, index, value)
      case headBlock =>
        blocksDQ.removeHead()
        if (headBlock.free != 0)
          blocksDQ.prepend(headBlock.freeIt)
        //print(headBlock.id.toString*fileLength)
        computeRec(blocksDQ, index + headBlock.fileLength, value + headBlock.sumAt(index))

  computeRec(mutable.ArrayDeque.from(blocks), 0, 0L)
