import java.io.PrintStream

import scala.io.Source

object Main2 {
  def solve(d: Int, horses: Vector[(Int, Int)]): Double = {
    //1. 가장 늦은 말의 속도보다 빠를 수 없다.
    //2. 그 속도 보다 빠른 어떤 말의 포지션을 넘을 수 없다.
    val head = horses.head
    var idealSpeed: Double = head._2 / (d - head._1).toDouble * d
    for {
      (pos, speed) <- horses.tail
      if speed < idealSpeed //애니 속도가 더 느린 경우 채크 필요 없음.
    } {
      val spd = idealSpeed - speed
      val time = pos.toDouble / spd
      if (pos + speed * time <= d) {
        //도착지를 넘어서면 채크 필요 없음.
        idealSpeed = Math.min(idealSpeed, speed / (d - pos).toDouble * d)
      }
    }
    idealSpeed
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
        val Array(d, n) = itr.next().split(' ').map(_.toInt)
        val horeses = Array.tabulate(n) {
          _ =>
            val Array(pos, speed) = itr.next().split(' ').map(_.toInt)
            (pos, speed)
        }

        stream.println(f"Case #$set: ${solve(d, horeses.toVector)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}