import scala.annotation.tailrec
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val blocks = inputLines.head.grouped(2).zipWithIndex.toArray.map:
      case (size, index) if size.length == 1 => Block(index, size.head.asDigit, 0)
      case (size, index) => Block(index, size.head.asDigit, size.last.asDigit)

    val result1 = s"${compute(blocks)}"
    val result2 = s"${computePart2(blocks)}"

    (s"$result1", s"$result2")


case class Block(id: Int,fileLength: Int, free: Int):
  lazy val total: Int = fileLength + free
  def sumAt(startIndex: Int): Long = id * ((2 * startIndex + fileLength - 1) * fileLength / 2f).toLong

  def addFree(freeToAdd: Int): Block = this.copy(free = free + freeToAdd)

  def freeIt: Block = this.copy(fileLength = 0)

  override def toString: String = id.toString*fileLength + "."*free

def compute(blocks: Array[Block]): Long =
  @tailrec
  def computeRec(blocksDQ: mutable.ArrayDeque[Block], index: Int, value: Long): Long =
    blocksDQ.head match
      case Block(_, 0, freeInHead) =>
        blocksDQ.length match
          case 1 => value
          case _ =>
            blocksDQ.removeHead()
            blocksDQ.last match
              case last @ Block(_, fileLengthInLast, _) if fileLengthInLast <= freeInHead =>
                blocksDQ.removeLast()
                blocksDQ.prepend(last.copy(free = freeInHead - fileLengthInLast))
              case last @ Block(_, fileLengthInLast, _) =>
                blocksDQ.removeLast()
                blocksDQ.append(last.copy(fileLength = fileLengthInLast - freeInHead))
                blocksDQ.prepend(last.copy(fileLength = freeInHead, free = 0))
            computeRec(blocksDQ, index, value)
      case headBlock =>
        blocksDQ.removeHead()
        if (headBlock.free != 0)
          blocksDQ.prepend(headBlock.freeIt)
        computeRec(blocksDQ, index + headBlock.fileLength, value + headBlock.sumAt(index))

  computeRec(mutable.ArrayDeque.from(blocks), 0, 0L)

def computePart2(blocks: Array[Block]): Long =
  @tailrec
  def computeRec(blocksDQ: mutable.ArrayDeque[Block], index: Int): Array[Block] =
    index match
      case 0 => blocksDQ.toArray
      case blockMovingIndex =>
        val blockMoving = blocksDQ(blockMovingIndex)
        val freePlaceToFind = blockMoving.fileLength
        val found = blocksDQ.take(index).zipWithIndex.find(_._1.free >= freePlaceToFind)
        found match
          case None => computeRec(blocksDQ, index - 1)
          case Some(receivingBlock, receivingBlockIndex) =>
            blocksDQ.update(receivingBlockIndex, receivingBlock.copy(free = 0))
            blocksDQ.remove(blockMovingIndex)
            blocksDQ.insert(receivingBlockIndex + 1, blockMoving.copy(free = receivingBlock.free - blockMoving.fileLength))
            blocksDQ.update(blockMovingIndex, blocksDQ(blockMovingIndex).addFree(blockMoving.total))
            computeRec(blocksDQ, index)

  computeRec(mutable.ArrayDeque.from(blocks), blocks.length - 1).foldLeft((0L, 0)):
    case (acc, block) =>
      (acc._1 + block.sumAt(acc._2), acc._2 + block.total)
  ._1