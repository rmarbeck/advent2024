object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val blocks = inputLines.head.grouped(2).zipWithIndex.toArray.map:
      case (size, index) => Block(index, size.head.toInt, size.last.toInt)

    blocks.foldLeft((0L, 0, blocks.length - 1)):
      case (acc, block) =>
        block match
          case Block(id, fileLength, free) =>
            (acc._1 + id * sum(acc._2, fileLength), acc._2 + fileLength, 


    val result1 = s""
    val result2 = s""

    (s"$result1", s"$result2")


case class Block(id: Int,fileLength: Int, free: Int)

def sum(from: Int, length: Int): Long = (to + from) / 2

def compute(blocks: Array[Block], currentId: Int, lastBlockNotEmpty: Int, currentValue: Long) : Long =
  def moveLeft(freePlaces: Int): Unit =
    
  val Block(_, fileLength, free) = blocks(currentId)
  moveLeft(free)