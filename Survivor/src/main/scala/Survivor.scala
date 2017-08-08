import java.io.PrintStream

import scala.io.Source

object Survivor {
  def solve(list: Vector[(Int, Int)]) = {
    def find(maxTime:Int, list:List[(Int, Vector[(Int,Int)])]):Int = list match {
      case Nil => maxTime
      case (time, rest) :: ll if rest.isEmpty => find(time max maxTime, ll)
      case (time, rest) :: ll =>
        val (x,xs) = (rest.head, rest.tail)
        val newTime = time + x._2
        find(maxTime, (newTime, xs.filterNot(_._1 < newTime)) :: (time,xs) :: ll)
    }
    find(0, List((0,list)))
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
        val foods = itr.next().toInt

        val ret = (for {
          i <- (0 until foods).toVector
        } yield {
          val Array(p, s) = itr.next().split(' ').map(_.toInt)
          (p, s)
        }).sortWith((x,y) => x._1 + x._2 < y._1 + y._2)

        stream.println(f"Case #$set: ${solve(ret)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}

