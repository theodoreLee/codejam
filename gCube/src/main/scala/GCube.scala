import java.io.FileOutputStream
import scala.io.Source

object GCube {
  def solve(itr: Iterator[String]) = {
    val Array(_, m) = itr.next().split(' ').map(_.toInt)
    val array = itr.next().split(' ').map(_.toInt).map(Math.log(_))

    (0 until m).map { _ =>
      val Array(l, r) = itr.next().split(' ').map(_.toInt)
      val sub = array.slice(l, l + r - l + 1)
      Math.exp(sub.sum / sub.length)
    }
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new FileOutputStream(OUTPUT)
    try {
      Console.withOut(stream) {
        val sets = itr.next().toInt
        (1 to sets).foreach { set =>
          println(f"Case #$set:\n${solve(itr).mkString("\n")}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}