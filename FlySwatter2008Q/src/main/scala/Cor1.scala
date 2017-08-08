import java.io.PrintStream

import scala.io.Source

object Cor1 {
  def solve(set:Int, itr: Iterator[String]) = {
    println("-"*50 + set)
    val Array(f, or, t, r, g) = itr.next().split(' ').map(_.toDouble)
    val R = or
    val RSquare = (R - t - f) * (R - t - f)
    def intersectionDc(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
      assert(x2 > x1 && y2 > y1, s"$x2, $x1, $y2, $y1, ${(x2 - x1) * (y2 - y1)}")
      if (x1 * x1 + y1 * y1 >= RSquare) 0.0
      else {
        val precision = 1e-7
        if (x2 * x2 + y2 * y2 <= RSquare) {
          (x2 - x1) * (y2 - y1)
        } else if ((x2 - x1) * (y2 - y1) <= precision) {
          (x2 - x1) * (y2 - y1) / 2
        } else {
          val xm = (x1 + x2) / 2
          val ym = (y1 + y2) / 2
          intersectionDc(x1, y1, xm, ym) +
            intersectionDc(xm, y1, x2, ym) +
            intersectionDc(x1, ym, xm, y2) +
            intersectionDc(xm, ym, x2, y2)
        }
      }
    }

    if (2 * f >= g) 1.0
    else {
      //1
      var sum = 0.0
      for {
        x1 <- r + f until R - t - f by 2 * r + g
        y1 <- r + f until R - t - f by 2 * r + g
      } {
        val x2 = x1 + g - 2 * f
        val y2 = y1 + g - 2 * f
        sum += intersectionDc(x1, y1, x2, y2)
      }
      println(sum , or * or * Math.PI / 4)
      1 - sum / (or * or * Math.PI / 4)
    }
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "test.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve(set, itr)}%.6f")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}