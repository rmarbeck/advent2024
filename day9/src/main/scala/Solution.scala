import scala.annotation.tailrec
import scala.collection.mutable

type Deque = mutable.ArrayDeque[Block]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val blocks = inputLines.head.grouped(2).zipWithIndex.toArray.map:
      case (size, index) => Block(index, size.head.asDigit, size.last.asDigit)

    val result1 = s"${computePart1(blocks)}"
    val result2 = s"${computePart2(blocks)}"

    (s"$result1", s"$result2")


case class Block(id: Int,fileLength: Int, free: Int):
  lazy val total: Int = fileLength + free
  def sumAt(startIndex: Int): Long = id * ((2 * startIndex + fileLength - 1) * fileLength / 2f).toLong

  def markFull: Block = this.copy(free = 0)
  def freeIt: Block = this.copy(fileLength = 0)
  def addFree(freeToAdd: Int): Block = this.copy(free = free + freeToAdd)
  def fitsIn(other: Block): Boolean = this.fileLength <= other.free
  def moveInFreeSpaceOf(other: Block): Block = this.copy(free = other.free - fileLength)
  def removeMoveable(toMove: Int): Block = this.copy(fileLength = fileLength - toMove)
  def moveAsMuchAsPossibleInFreeSpaceOf(other: Block): Block = this.copy(fileLength = other.free).markFull

  override def toString: String = id.toString*fileLength + "."*free

def computePart1(blocks: Array[Block]): Long =
  @tailrec
  def computeRec(index: Int, value: Long)(using blocksDQ: Deque): Long =
    blocksDQ.head match
      case head @ Block(_, 0, freeInHead) =>
        blocksDQ.length match
          case 1 => value
          case _ =>
            blocksDQ.removeHead()
            blocksDQ.last match
              case last if last.fitsIn(head) =>
                blocksDQ.removeLast()
                blocksDQ.prepend(last.moveInFreeSpaceOf(head))
              case last =>
                blocksDQ.removeLast()
                blocksDQ.append(last.removeMoveable(freeInHead))
                blocksDQ.prepend(last.moveAsMuchAsPossibleInFreeSpaceOf(head))
            computeRec(index, value)
      case head =>
        blocksDQ.removeHead()
        if (head.free != 0)
          blocksDQ.prepend(head.freeIt)
        computeRec(index + head.fileLength, value + head.sumAt(index))

  computeRec(0, 0L)(using mutable.ArrayDeque.from(blocks))

def computePart2(blocks: Array[Block]): Long =
  @tailrec
  def computeRec(indexOfBlockToMove: Int)(using blocksDQ: Deque): Deque =
    indexOfBlockToMove match
      case 0 => blocksDQ
      case _ =>
        val blockMoving = blocksDQ(indexOfBlockToMove)
        val freePlaceToFind = blockMoving.fileLength
        val placeToMoveBlock = blocksDQ.take(indexOfBlockToMove).zipWithIndex.find(_._1.free >= freePlaceToFind)
        placeToMoveBlock match
          case None => computeRec(indexOfBlockToMove - 1)
          case Some(receivingBlock, receivingBlockIndex) =>
            blocksDQ.update(receivingBlockIndex, receivingBlock.markFull)
            blocksDQ.remove(indexOfBlockToMove)
            blocksDQ.insert(receivingBlockIndex + 1, blockMoving.moveInFreeSpaceOf(receivingBlock))
            val indexOfBlockJustBeforeTheOneMoved = indexOfBlockToMove
            blocksDQ.update(indexOfBlockJustBeforeTheOneMoved, blocksDQ(indexOfBlockJustBeforeTheOneMoved).addFree(blockMoving.total))
            computeRec(indexOfBlockJustBeforeTheOneMoved)

  given Deque = mutable.ArrayDeque.from(blocks)
  val (checkSum, _) = computeRec(blocks.length - 1).foldLeft((0L, 0)):
    case ((acc, index), block) =>
      (acc + block.sumAt(index), index + block.total)

  checkSum