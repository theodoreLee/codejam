import java.io.PrintStream

import scala.io.Source

object Main {
  def solve(set: Int, itr: Iterator[String]) = {
    println(set + "-" * 50)
    val Array(y, x) = itr.next().split(' ').map(_.toInt)
    val arr = Array.tabulate(y)(_ => itr.next().split(' ').map(_.toInt))
    val ret: Array[Array[Char]] = Array.fill(y, x)('*')

    var basin = 'a'
    def findSink(c: (Int, Int), path: List[(Int, Int)]):Unit = {
      val cur = ret(c._1)(c._2)
      if (cur != '*') {
        path.foreach { case (_y, _x) => ret(_y)(_x) = cur }
      } else {
        val next = search(c._1, c._2, arr(c._1)(c._2))
        if (next == (-1, -1)) {
          ret(c._1)(c._2) = basin
          path.foreach { case (_y, _x) => ret(_y)(_x) = basin }
          basin = (basin + 1).toChar
        } else {
          findSink(next, c :: path)
        }
      }
    }

    def search(i: Int, j: Int, m: Int) = {
      val pos = Array((-1, 0), (0, -1), (0, 1), (1, 0))
      var min = m
      var p = (-1, -1)
      for {
        (yd, xd) <- pos
      } {
        val _i = i + yd
        val _j = j + xd
        if (-1 < _i && _i < y && -1 < _j && _j < x && arr(_i)(_j) < min) {
          min = arr(_i)(_j)
          p = (_i, _j)
        }
      }
      p
    }

    for {
      i <- 0 until y
      j <- 0 until x
      if ret(i)(j) == '*'
    } {
      findSink((i,j), Nil)
    }
    ret.map(_.mkString(" ")).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set:\n${solve(set, itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}

//https://www.facebook.com/v2.5/dialog/oauth?response_type=code&client_id=1488919094686229&redirect_uri=https//www.tuktak.net/v1/authenticate/facebook&display=popup&scope=email