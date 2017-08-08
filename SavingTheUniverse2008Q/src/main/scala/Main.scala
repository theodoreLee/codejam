import java.io.PrintStream
import scala.io.Source

object Main {
  def solve(itr: Iterator[String]) = {
    val s = itr.next().toInt
    val engines = (0 until s).map(_ => itr.next()).toSet
    val q = itr.next().toInt
    var ret = 0
    var _en = engines
    (0 until q).foreach {_ =>
      val query = itr.next()
      _en = _en - query
      if (_en.isEmpty) {
        ret += 1
        _en = engines - query
      }
    }
    ret
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
        stream.println(f"Case #$set: ${solve(itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}