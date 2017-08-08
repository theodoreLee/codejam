import java.io.PrintStream
import java.lang.Math._

import scala.io.Source

object Trigo {
  def circularSegment(rad: Double, th: Double) = {
    assert(th > 0, s"$th, ${sin(th)}")
    rad * rad * (th - sin(th)) / 2
  }

  def solve(set: Int, itr: Iterator[String]) = {
    val Array(f, _R, t, r, g) = itr.next().split(' ').map(_.toDouble)
    val R = _R
    val rad = R - t - f
    val rSquare = rad * rad
    println("-"* 50)

    var sum = 0.0
    if (g > 2 * f) {
      for {
        x1 <- (r + f) until rad by g + 2 * r
        y1 <- (r + f) until rad by g + 2 * r
      } {
        val x2 = x1 + g - f * 2
        val y2 = y1 + g - f * 2
        assert(x2 > x1)
        assert(y2 > y1)
        val x1y1 = x1 * x1 + y1 * y1
        val x2y1 = x2 * x2 + y1 * y1
        val x1y2 = x1 * x1 + y2 * y2
        val x2y2 = x2 * x2 + y2 * y2
        if (rSquare >= x2y2) {
          //모두가 안에 있는 경우
          sum += (x2 - x1) * (y2 - y1)
        } else if (rSquare > x1y1) {
          if (x1y2 >= rSquare && x2y1 >= rSquare) {
            //오직  p(x1, y1) 만 안에 있는 경우
            val a = circularSegment(rad, acos(x1 / rad) - asin(y1 / rad))
            val b = (sqrt(rSquare - x1 * x1) - y1) * (sqrt(rSquare - y1 * y1) - x1) / 2
            sum += a + b
            assert(a + b < (x2 - x1)*(y2-y1), f"$a%.6f $b%.6f ${(x2-x1)*(y2-y1)}%.6f")
          } else if (rSquare <= x1y2) { //x2y1 && x1y1
            assert(x2 / rad <= 1.0)
            val a = circularSegment(rad, acos(x1 / rad) - acos(x2/rad))
            val b = (x2 - x1) * (sqrt(rSquare - x1 * x1) + sqrt(rSquare - x2 * x2) - y1 * 2) / 2
            assert(a + b < (x2 - x1)*(y2-y1))
            sum += a + b
          } else if (rSquare <= x2y1) {
            //p(x1, y1) (x1, y2)
            assert(y2 / rad <= 1.0)
            val a = circularSegment(rad, asin(y2 / rad) - asin(y1 / rad))
            assert(sum != Double.NaN)
            assert(rad > y1)
            assert(rad > y2)
            val b = (y2 - y1) * (sqrt(rSquare - y1 * y1) + sqrt(rSquare - y2 * y2) - x1 * 2) / 2
            sum += a + b
            assert(a + b < (x2 - x1)*(y2-y1))
          } else {
            //p(x2,y2) 만 밖으로 나간 경우
            sum += circularSegment(rad, asin(y2 / rad) - acos(x2 / rad))
            assert(sum != Double.NaN)
            sum += (y2 - y1) * (x2 - x1) - (x2 - sqrt(rSquare - y2 * y2)) * (y2 - sqrt(rSquare - x2 * x2)) / 2
            assert(sum != Double.NaN)
          }
        }
      }
    }
    assert(sum >= 0, s"$set: $sum")
//    assert(sum < R * R * PI / 4, s"$set:$sum ${R * R * PI / 4}")
    1 - (sum / (R * R * PI / 4))
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