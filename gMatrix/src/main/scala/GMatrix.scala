import java.io.PrintStream

import scala.io.Source

object GMatrix {
  def deque(vector: Vector[(Int,Int)], accr:Vector[(Int,Int)] = Vector()):Vector[(Int,Int)] = vector match {
    case Vector() => accr
    case (x,_) +: _ if accr.nonEmpty && accr.last._1 <= x => deque(vector, accr.dropRight(1))
    case x +: xs => deque(xs, accr :+ x)
  }

  def findMaxVector(arr:Vector[Int], k:Int):IndexedSeq[Int] = {
    val a = arr.zipWithIndex

    def _solve(arr:Vector[(Int,Int)], d:Vector[(Int,Int)], i:Int, ret:Vector[Int]):Vector[Int] = arr match {
      case Vector() => ret :+ d.head._1
      case x +: xs =>
        val newRet = ret :+ d.head._1
        _solve(xs, deque(Vector(x), d.dropWhile(_._2 < i - k + 1)), i+1, newRet)
    }
    _solve(a.drop(k), deque(a take k), k, Vector())
  }

  def solve(a: Array[Int], b: Array[Int], c: Int, mod: Int, n: Int, k: Int): Long = {
    val range = a.indices.toVector
    val ret = for {
      i <- range
      row = range.map(j => (a(i) * (i + 1) + b(j) * (j + 1) + c) % mod)
    } yield findMaxVector(row, k)

    (0 to (n - k)).foldLeft(0L) {
      case (sum, i) => sum + findMaxVector(ret.map(_(i)), k).map(_.toLong).sum
    }
  }


  def main(args: Array[String]): Unit = {
    val INPUT = "D-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out2"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        println("-" * 20, set, "-" * 20)
        val Array(n, k, c, x) = itr.next().split(" ").map(_.toInt)
        val a = itr.next().split(" ").map(_.toInt)
        val b = itr.next().split(" ").map(_.toInt)
        stream.println(f"Case #$set: ${solve(a, b, c, x, n, k)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}