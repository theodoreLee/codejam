import java.io.PrintStream
import scala.io.Source

object Main {
  def solve(itr: Iterator[String]) = {
    val t = itr.next().toInt
    val Array(na, nb) = itr.next().split(' ').map(_.toInt)

    def timeTable(n:Int): List[(Int,Int)] = {
      (0 until n).map(_ => {
        val Array(dh, dm, ah, am) =itr.next().split(" |:").map(_.toInt)
        val d = dh * 60 + dm
        val a = ah * 60 + am + t
        (d, a)
      }).sortBy(_._1).toList
    }

    val a = timeTable(na)
    val b = timeTable(nb)

    def greedy(a:List[Int], avail:List[Int], accr:Int):Int = a match {
      case Nil => accr
      case x :: xs if avail.isEmpty => greedy(xs, avail, accr + 1)
      case x :: xs if avail.head <= x => greedy(xs, avail.tail, accr)
      case x :: xs => greedy(xs, avail, accr + 1)
    }

    val retA = greedy(a.map(_._1), b.map(_._2).sorted, 0)
    val retB = greedy(b.map(_._1), a.map(_._2).sorted, 0)
    s"$retA $retB"
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
        stream.println(f"Case #$set: ${solve(itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}