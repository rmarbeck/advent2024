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
          dirX <- List(-1, 0, 1) // exploring in each direction on x
          dirY <- List(-1, 0, 1) // exploring in each direction on y
        yield
          val List(driftX, driftY) = List(dirX, dirY).map(dir => List(1 ,2 ,3).map(_ * dir)) // looking for 3 letters
          val word = driftX.zip(driftY).map((curX, curY) => readChar(x + curX, y + curY)).mkString("")
          word == "MAS"
        ).count(identity)
      ).sum


    val result2 =
      (for
        x <- grid.indices.tail // no need to explore first row
        y <- grid(x).indices.tail // no need to explore first col
        if grid(x)(y) == 'A'
      yield
        val words =
          val driftX = List(1, -1, -1, 1)  // looking for 4 letters around
          val driftY = List(-1, 1, -1, 1)  // diagonally
          driftX.zip(driftY).map:
            case (curX, curY) => readChar(x + curX, y + curY)
          .mkString("").grouped(2)

        words.forall(word => word.contains('S') && word.contains('M'))
      ).count(identity)

    (s"$result1", s"$result2")



