import java.io.FileOutputStream
import scala.io.Source

object DrearyDesign {
  def solve(str: String) = {
    val Array(k, v) = str.split(' ').map(_.toInt)

    def bSize(r:Int, g:Int):Long = (((r max g) - v max 0) to ((r min g) + v min k)).size.toLong

    def gbSize(r:Int):Long = ((r - v max 0) to (r + v min k)).map(bSize(r,_)).sum

    val middleRange = v to (k - v)
    middleRange.headOption match {
      case Some(r) =>
        val middleTotal = gbSize(r) * middleRange.size
        val headTotal = (0 until v).map(gbSize).sum
        headTotal * 2 + middleTotal
      case None => (0 to k).map(gbSize).sum
    }
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-large-practice-2.in"
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