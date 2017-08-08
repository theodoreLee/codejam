import java.io.FileOutputStream
import scala.io.Source

object Googlander {
  val R = 25
  val C = 25

  val array:Array[Array[Long]] = {
    val arr = Array.ofDim[Long](R,C)
    for {
      x <- arr.indices
      y <- arr(x).indices
    } {
      if(x == 0 || y == 0) arr(x)(y) = 1
      else arr(x)(y) = arr(x-1)(y) + arr(x)(y-1)
    }
    arr
  }

  def solve(line:String) = {
    val Array(r,c) = line.split(' ').map(_.toInt)
    array(r - 1)(c - 1)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "D-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new FileOutputStream(OUTPUT)
    try {
      Console.withOut(stream) {
        val sets = itr.next().toInt
        (1 to sets).foreach { set =>
          println(f"Case #$set: ${solve(itr.next())}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}