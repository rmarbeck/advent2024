import scala.annotation.tailrec
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val blocks = inputLines.head.grouped(2).zipWithIndex.toArray.map:
      case (size, index) => Block(index, size.head.asDigit, size.last.asDigit)

    println(compute(blocks))
    println("\n"+computePart2(blocks))

    val result1 = s""
    val result2 = s""

    (s"$result1", s"$result2")


case class Block(id: Int,fileLength: Int, free: Int):
  def splitAt(length: Int): (Block, Block) = (this.copy(fileLength = length, free = 0), this.copy(fileLength = fileLength - length, free = free))
  def sumAt(startIndex: Int): Long = id * ((2 * startIndex + fileLength - 1) * fileLength / 2f).toLong

  def expandFrom(other: Block): Block = this.copy(free = this.free + other.fileLength + other.free)

  def freeIt: Block = this.copy(fileLength = 0)

  override def toString: String = id.toString*fileLength + "."*free

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

def computePart2(blocks: Array[Block]): Long =
  @tailrec
  def computeRec(blocksDQ: mutable.ArrayDeque[Block], index: Int): Array[Block] =
    blocksDQ.foreach(print)
    println
    index match
      case 0 => blocksDQ.toArray
      case value =>
        val freePlaceToFind = blocksDQ(value).fileLength
        println(freePlaceToFind)
        val found = blocksDQ.zipWithIndex.filter(_._2 < index).find(_._1.free >= freePlaceToFind)
        found match
          case None => computeRec(blocksDQ, index - 1)
          case Some(block, innerIndex) =>
            blocksDQ.update(innerIndex, block.copy(free = 0))
            blocksDQ.insert(innerIndex + 1, blocksDQ(value).copy(free = block.free - freePlaceToFind))
            blocksDQ.remove(index + 1)
            blocksDQ.update(index, blocksDQ(index).copy(free = blocksDQ(index).free + blocksDQ(value).fileLength + blocksDQ(value).free ))
            computeRec(blocksDQ, blocksDQ.length - 1)

  computeRec(mutable.ArrayDeque.from(blocks), blocks.length - 1).zipWithIndex.foldLeft(0L):
    case (acc, (block, index)) =>
      print(block)
      acc + block.sumAt(index)
