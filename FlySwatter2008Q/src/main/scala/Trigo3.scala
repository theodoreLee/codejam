import java.io.PrintStream
import java.lang.Math._

import scala.io.Source

object Trigo3 {
  def solve(set: Int, itr: Iterator[String]) = {
    println("-" * 50)
    val Array(f, _R, t, r, g) = itr.next().split(' ').map(_.toDouble)
    val rad = _R - f - t
    val radSquare = rad * rad

    def circularSegment(th: Double) = radSquare * (th - sin(th)) / 2

    //이부분도 오류 가능성 존재함. sqrt 하고 안하고.
    def dist(x:Double, y:Double) = x * x + y * y

    def pytha(side:Double) = sqrt(radSquare - side * side)

    // 1/4분면만 검사.
    var sum = 0.0
    val square = (g - 2 * f) * (g - 2 * f)
    if (2 * f < g) {
      //작거나 같으면 무조건 0
      for {
        x1 <- (r + f) until rad by g + 2 * r
        y1 <- (r + f) until rad by g + 2 * r
      } {
        val x2 = x1 + g - 2 * f
        val y2 = y1 + g - 2 * f
        if(dist(x1, y1) < radSquare) {
          if (dist(x2, y2) < radSquare) { //4변
            //실제로는 사이즈가 같음..
            sum += (x2 - x1) * (y2 - y1)
          } else if (dist(x1, y2) < radSquare && dist(x2, y1) < radSquare) { //3변
            sum += circularSegment(asin(y2 / rad) - acos(x2 / rad))
            //사각형 - 삼각형
            sum += (x2 - x1) * (y2 - y1) - ((x2 - pytha(y2)) * (y2 - pytha(x2)) / 2)
          } else if (dist(x1, y2) < radSquare) { // 2변
            sum += circularSegment(asin(y2 / rad) - asin(y1 / rad))
            //사다리꼴
            sum += (y2 - y1) * (pytha(y2) + pytha(y1) - x1 * 2) / 2
          } else if (dist(x2, y1) < radSquare) { // 2변
            sum += circularSegment(acos(x1/ rad) - acos(x2/rad))
            //사다리꼴
            sum += (x2 - x1) * (pytha(x2) + pytha(x1) - y1 * 2) / 2
          } else { //1변
            sum += circularSegment(acos(x1 / rad) - asin(y1 / rad))
            sum += (pytha(x1) - y1) * (pytha(y1) - x1) / 2
          }
        }
      }
    }
    1 - sum / (_R * _R * PI / 4)
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