import java.io.PrintStream
import scala.io.Source

object GFiles {
  def solve(itr: Iterator[String]) = {
    println("="*50)
    val vector = (0 until itr.next().toInt).map({ _ =>
      val Array(per, files) = itr.next().split(' ').map(_.toLong)
      (per, files)
    }).toVector.dropWhile(_._1 == 0)

    def calculate(per: Long, files: Long) = (files * 100 / (per + 1), files * 100 / per)

    def _solve(vector: Vector[(Long, Long)], possibleMin: Long, possibleMax: Long): Long = vector match {
      case Vector() if possibleMax - possibleMin == 1 => possibleMax
      case Vector() => -1
      case (per, files) +: xs =>
        val (pMin, pMax) = calculate(per, files)
        _solve(xs, possibleMin max pMin, possibleMax min pMax)
    }

    if(vector.last._1 == 100) vector.last._2
    else {
      val (pMin, pMax) = calculate(vector.head._1, vector.head._2)
      _solve(vector.tail, pMin, pMax)
    }
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