import java.io.PrintStream
import scala.io.Source

object Main {
  def solve(set: Int, itr: Iterator[String]) = {
    val ret = Array.fill(19)(BigInt(0))
    val str = itr.next()
    val comp = "welcome to code jam"
    val len = comp.length
    for {
      c <- str
      i <- 0 until len
      if comp(i) == c
    } {
      if(i == 0) {
        ret(i) += 1
      } else {
        ret(i) += ret(i - 1)
      }
    }
    assert(ret(len - 1) >= 0, ret(len -1))
    ret(len - 1)
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
        val ret = solve(set, itr).toString
        val m = ret.drop(ret.length - 4)
        val x = 4 - m.length
        stream.println(f"Case #$set: ${"0"*x}$m")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}