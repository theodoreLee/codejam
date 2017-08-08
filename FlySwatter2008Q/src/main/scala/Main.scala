import java.io.PrintStream

import scala.io.Source

object Main {
  def solve(set: Int, itr: Iterator[String]) = {
    println("-" * 50 + set)
    val Array(f, _R, t, r, g) = itr.next().split(' ').map(_.toDouble)

    val rad = _R - t - f
    val RadSqaure = rad * rad

    //dcx => 파리가 지나갈수 있는 지점의 넓이를 리턴.
    //dc(x1, y1, x2, y2) = 사각형과 원(반지름 - 테두리 - 파리의 빤지름)의 1/4분면이 겹치는 넓이를 리턴.
    def dc(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
      /** 1 dc 이용 **/
      if (x1 * x1 + y1 * y1 > RadSqaure) 0.0
      else {
        //
        val precision = 1E-12

        if (x2 * x2 + y2 * y2 < RadSqaure) {
          //사각형이 1/4분면 안에 들어가는 경우
          (x2 - x1) * (y2 - y1)
        } else if ((x2 - x1) * (y2 - y1) < precision) {
          //사각형이 영향을 미치지 않을 정도로 충분히 작을때...
          (x2 - x1) * (y2 - y1) / 2
        } else {
          //일부분만 영역에 포함될 때..
          val mx = (x1 + x2) / 2
          // 1.0 3.0 => 2.0
          val my = (y1 + y2) / 2 // 1.0 3.0 => 2.0
          dc(x1, y1, mx, my) +
            dc(x1, my, mx, y2) +
            dc(mx, y1, x2, my) +
            dc(mx, my, x2, y2)
        }
      }
    }

    //s(X) = 파리를 잡을 수 있는 확률
    //s(x) = 1 - 파리를 놓치는 부분 / 전체 부분.
    //s(x) = 1 - f(~)/ (r*r*pi / 4)
    if (2 * f >= g) 1.0 //놓칠수 없음
    else {
      var sum = 0.0
      for {
        x1 <- r + f until rad by 2 * r + g
        y1 <- r + f until rad by 2 * r + g
      } {
        val x2 = x1 + g - 2 * f
        val y2 = y1 + g - 2 * f
        assert(x2 > x1)
        assert(y2 > y1)
        sum += dc(x1, y1, x2, y2)
      }
      1.0 - sum / (_R * _R * Math.PI / 4)
    }
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