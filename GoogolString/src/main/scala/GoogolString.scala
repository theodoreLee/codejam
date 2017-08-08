import java.io.FileOutputStream
import scala.io.Source


object GoogolString {

  def comp(sign:Boolean, value:Boolean):Boolean = if(sign) value else !value


  def findKth(k:Long, sign:Boolean):Boolean = {
    val ln: Double = log(k,2)
    val ceil: Double =  Math.ceil(ln)
    if(ln == ceil) comp(sign, false)
    else findKth((1L << ceil.toInt) - k, !sign)
  }


  def log(x: Double, base: Double):Double = Math.log(x) / Math.log(base)


  def solve(str:String) = {
    val k = str.toLong
    Math.exp()
    if(findKth(k, true)) 1 else 0
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-large-practice.in"
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