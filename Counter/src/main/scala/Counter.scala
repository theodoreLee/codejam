import java.io.FileOutputStream

import scala.io.Source

/**
 * Created by teddy on 5/4/15.
 */
object Counter {
  val INPUT = "A-large-practice.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"


  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val sets = itr.next().toInt

    val writer = new FileOutputStream(OUTPUT)
//    val writer = Console.out
    try {
      Console.withOut(writer) {
        for (set <- 1 to sets) {
          println(f"Case #${set}: ${solve(itr.next())}")
        }
      }
    } finally {
      writer.close()
      System.exit(0)
    }
  }

  def solve(n:String) = {
    val N = n.toLong
    val maxPos = n.length
    def counter(accr:String, count:Long, pos:Int):Long = accr match {
      case _ if N == accr.toLong => count
      case _ if pos == maxPos =>
        // 현재 100 이기 때문에 -1을 한경우 자리수가 줄 염려는 없음
        val comp = if(n.last == '0') (N - 1).toString else n
        val newAccr = "1"+"0"*(n.length /2 -1) + comp.take((n.length +1)/2).reverse
        if(newAccr.reverse == newAccr || newAccr.reverse.toLong > N)
          count + n.toLong - accr.toLong
        else count + (N - newAccr.reverse.toLong) + newAccr.toLong - accr.toLong + 1
      case _ if pos + 1 == maxPos =>
        val newAccr = "1" + "0" * (accr.length)
        counter(newAccr, count + newAccr.toLong - accr.toLong, pos + 1)
      case _ if pos < maxPos =>
        val newAccr = "1" + "0" * (accr.length/2) + "9" * ((accr.length + 1) / 2)
        counter(newAccr.reverse, count + newAccr.toLong - accr.toLong + 1, pos + 1)
    }
    counter("1", 1, 1)
  }
}
