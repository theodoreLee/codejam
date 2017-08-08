import java.io.PrintStream

import scala.io.Source

object Main {
  def solve(number: Long) = {
    def tidyNumber(number: Long, accr: String = ""): String = {
      if(number == 0) accr
      else {
        val head = number / 10
        val res = number % 10
        if (accr.head - '0' < res) {
          tidyNumber(head, (res - 1) + "9" * accr.length)
        } else {
          tidyNumber(head, res + accr)
        }
      }
    }
    tidyNumber(number / 10, (number % 10).toString).toLong
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-large.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve(itr.next().toLong)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}