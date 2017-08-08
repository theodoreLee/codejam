import java.io.FileOutputStream

import scala.io.Source

object HairCut {
  val INPUT = "B-large-practice.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
  val isConsole = false

  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new FileOutputStream(OUTPUT)
    try {
      Console.withOut(stream) {
        val sets = itr.next().toInt
        (1 to sets).foreach { set =>
          val Array(_, n) = itr.next().split(' ').map(_.toLong)
          val barbers = itr.next().split(' ').map(_.toInt).toVector
          println(s"Case #$set: ${solve(barbers, n)}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }

  def solve(barbers: Vector[Int], n: Long) = {
    val maxMinutes = (barbers.max * n.toLong) / barbers.length
    //n - 1이 앉아있는 시간을 찾는다.
    val (minutes, target) = roughBinarySearch(maxMinutes, n - 1)(m => barbers.map(b => (m / b.toDouble).ceil.toLong).sum)
    barbers.map {
      case b if minutes % b == 0 => 0
      case b => b - (minutes % b)
    }.zipWithIndex.sortBy(_._1).drop((n - 1 - target).toInt).head._2 + 1
  }

  
  def roughBinarySearch(max: Long, key: Long)(searchBy: Long => Long) = {
    import scala.annotation.tailrec

    @tailrec
    def search(lo: Long, hi: Long):(Long, Long) = {
      if (lo > hi) (hi, searchBy(hi)) // lo를 넘겨주면 찾는값보다 큰 수, hi를 넘겨주면 찾는 값보다 작은 수.
      else {
        val mid = lo + (hi - lo) / 2
        val target = searchBy(mid)
        target match {
          case _ if target == key => (mid, target)
          case _ if target < key => search(mid + 1, hi)
          case _ => search(lo, mid - 1)
        }
      }
    }
    search(0, max)
  }
}