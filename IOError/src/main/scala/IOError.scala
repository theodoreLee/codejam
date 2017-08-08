import java.io.FileOutputStream
import scala.io.Source

object IOError {

  def toBinaryChar(str:String):Char = Integer.parseInt(str, 2).toChar

  def solve(seq: Seq[String]) = {
    val str = seq(1).replaceAll("I","1").replaceAll("O","0")
    str.grouped(8).map(toBinaryChar).mkString
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-small-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = true

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new FileOutputStream(OUTPUT)
    try {
      Console.withOut(stream) {
        val sets = itr.next().toInt
        (1 to sets).foreach { set =>
          val seq = Seq(itr.next(),itr.next())
          println(f"Case #$set: ${solve(seq)}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}