import java.io.PrintStream

import scala.io.Source

object Main {
  def solve(set: Int, row: Int, col: Int, cakes: Array[Array[Char]]) = {
    def checkChar(sr: Int, er: Int, sc: Int, ec: Int): Either[(Int, Int), Char] = {
      var char = '?'
      for {
        i <- sr to er
        j <- sc to ec
      } {
        if (cakes(i)(j) != '?') {
          if (char == '?') {
            char = cakes(i)(j)
          } else if (char != cakes(i)(j)) {
            return Left((i, j))
          } else {
            assert(assertion = false, "something wrong..")
          }
        }
      }
      Right(char)
    }

    def dc(sr: Int, er: Int, sc: Int, ec: Int): Unit = {
      if (sr > er || sc > ec) assert(false)
      else {
        checkChar(sr, er, sc, ec) match {
          case Right('?') => assert(false)
          case Right(c) =>
            for {
              i <- sr to er
              j <- sc to ec
            } {
              cakes(i)(j) = c
            }
          case Left((i, j)) =>
            if (sr < i) {
              try {
                dc(sr, i - 1, sc, ec)
                dc(i, er, sc, ec)
              } catch {
                case _: Throwable =>
                  dc(sr, er, sc, j - 1)
                  dc(sr, er, j, ec)
              }
            } else {
              dc(sr, er, sc, j - 1)
              dc(sr, er, j, ec)
            }
        }
      }
    }

    dc(0, row - 1, 0, col - 1)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        val Array(row, col) = itr.next().split(' ').map(_.toInt)
        val cakes = (0 until row).map(_ => itr.next().toArray).toArray
        stream.println(f"Case #$set:")
        solve(set, row, col, cakes)
        cakes.foreach(i => stream.println(i.mkString("")))

      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}