import java.io.PrintStream
import java.lang.Math._

import scala.io.Source

object Trigo2 {
  def solve(set: Int, itr: Iterator[String]) = {
    println("-" * 50)
    val Array(f, _R, t, r, g) = itr.next().split(' ').map(_.toDouble)
    val R = _R
    val rad = R - t - f
    val rSquare = rad * rad

    def circularSegment(th: Double) = {
      //      assert(th > 0, th)
      rad * rad * (th - sin(th)) / 2
    }

    var sum = 0.0
    if (g > 2 * f) {
      val square = (g - 2 * f) * (g - 2 * f)
      for {
        x1 <- (r + f) until rad by g + 2 * r
        y1 <- (r + f) until rad by g + 2 * r
      } {
        val x2 = x1 + g - 2 * f
        val y2 = y1 + g - 2 * f
        assert(x1 < x2)
        assert(y1 < y2)
        if (x1 * x1 + y1 * y1 < rSquare) {
          if (x2 * x2 + y2 * y2 <= rSquare) {
            assert(x2 * x2 + y2 * y2 < rSquare)
            assert(x2 * x2 + y1 * y1 < rSquare)
            assert(x1 * x1 + y2 * y2 < rSquare)
            assert(x1 * x1 + y1 * y1 < rSquare)
            //            if (set < 5) println(4)
            //4변
            sum += square
          } else if (x1 * x1 + y2 * y2 >= rSquare && x2 * x2 + y1 * y1 >= rSquare) {
            assert(x2 * x2 + y2 * y2 > rSquare)
            assert(x2 * x2 + y1 * y1 > rSquare)
            assert(x1 * x1 + y2 * y2 > rSquare)
            assert(x1 * x1 + y1 * y1 < rSquare)
            //3변을 하면 == 부분.. 확률상 아주 적지만. == 부분이 걸림으로 처리가 안됨.
            //1변
            sum += circularSegment(acos(x1 / rad) - asin(y1 / rad))
            //삼각형
            val rest = (sqrt(rSquare - x1 * x1) - y1) * (sqrt(rSquare - y1 * y1) - x1) / 2
            assert(rest < square)
            sum += rest
          } else if (x1 * x1 + y2 * y2 >= rSquare) {
            assert(x2 * x2 + y2 * y2 > rSquare)
            assert(x2 * x2 + y1 * y1 < rSquare)
            assert(x1 * x1 + y2 * y2 > rSquare)
            assert(x1 * x1 + y1 * y1 < rSquare)
            //            if (set < 5) println("x2y1")
            //x1y1 x2y1
            sum += circularSegment(acos(x1 / rad) - acos(x2 / rad))
            //사다리꼴
            val rest = (x2 - x1) * (sqrt(rSquare - x2 * x2) + sqrt(rSquare - x1 * x1) - y1 * 2) / 2
            assert(rest < square, s"$rest $square $x1 $x2 $y1 $y2 $rad")
            sum += rest
          } else if (x2 * x2 + y1 * y1 >= rSquare) {
            assert(x2 * x2 + y2 * y2 > rSquare)
            assert(x2 * x2 + y1 * y1 > rSquare)
            assert(x1 * x1 + y2 * y2 < rSquare)
            assert(x1 * x1 + y1 * y1 < rSquare)
            //            if (set < 5) println("x1y2")
            //x1y1 x1y2
            sum += circularSegment(asin(y2 / rad) - asin(y1 / rad))
            //사다리꼴
            val rest = (y2 - y1) * (sqrt(rSquare - y2 * y2) + sqrt(rSquare - y1 * y1) - x1 * 2) / 2
            assert(rest < square)
            sum += rest
          } else {
            assert(x2 * x2 + y2 * y2 > rSquare)
            assert(x2 * x2 + y1 * y1 < rSquare)
            assert(x1 * x1 + y2 * y2 < rSquare)
            assert(x1 * x1 + y1 * y1 < rSquare)
            //            if (set < 5) println(3)
            //3변
            sum += circularSegment(asin(y2 / rad) - acos(x2 / rad))
            //사각형 - 삼각형
            val rest = square - (x2 - sqrt(rSquare - y2 * y2)) * (y2 - sqrt(rSquare - x2 * x2)) / 2
            assert(rest < square, f"$rest%.6f $square%.6f ")
            sum += rest
          }
        }
      }
    }
    1 - sum / (R * R * PI / 4)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "C-large-practice.in"
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