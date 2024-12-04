object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    val grid = inputLines.map(_.toCharArray).toArray

    def readChar(posX: Int, posY: Int): Char =
      if (grid.isDefinedAt(posX) && grid(posX).isDefinedAt(posY))
        grid(posX)(posY)
      else
        '.'

    val result1 =
      (for
        x <- grid.indices
        y <- grid(x).indices
        if grid(x)(y) == 'X'
      yield
        (for
          dirX <- List(-1, 0, 1)
          dirY <- List(-1, 0, 1)
        yield
          val List(posX, posY) = List(dirX, dirY).map(dir => List(1 ,2 ,3).map(_ * dir))
          val word = posX.zip(posY).map((curX, curY) => readChar(x + curX, y + curY)).mkString("")
          word match
            case "MAS" => 1
            case _ => 0
          ).sum
        ).sum


    val result2 =
      (for
        x <- grid.indices
        y <- grid(x).indices
        if grid(x)(y) == 'A'
      yield
        val words =
          val posX = List(1, -1, -1, 1)
          val posY = List(-1, 1, -1, 1)
          posX.zip(posY).map:
            case (curX, curY) => readChar(x + curX, y + curY)
          .mkString("").grouped(2)

        if words.forall(word => word.count(_ == 'S') == 1 && word.count(_ == 'M') == 1) then
          1
        else
          0
        ).sum

    (s"$result1", s"$result2")



