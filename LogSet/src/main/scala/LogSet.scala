import java.io.FileOutputStream
import scala.io.Source

object LogSet {

  def solve(seq: Seq[String]) = {
    val sPrime = seq.head.split(' ').map(_.toLong).reverse
    val frequencies = seq(1).split(' ').map(_.toLong).reverse

    def _solve(data: Map[Long, Long], accr: Map[Long, List[Long]]): List[Long] = data match {
      case _ if data.size == 1 && data.head._2 == 1 => accr(sPrime.head)
      case _ if data(data.keys.max) % 2 == 0 => _solve(data.mapValues(_ / 2), accr.mapValues(0 :: _))
      case _ =>
        val keys = data.keys.toVector.sortWith(_ > _)
        val diff = keys.head - keys(1)
        val newData = (data /: keys) {
          case (map, key) if map(key) == 0 => map
          case (map, key) => map.updated(key - diff, map(key - diff) - map(key))
        }.filterNot(_._2 == 0)

        //순서가 바뀌면 오답이 됩니다... 소 뒷걸음질 치다가 쥐잡은 격..
        val newMap = accr.map(x => (x._1 + diff, diff :: x._2)) ++ accr.mapValues(-diff :: _)
        _solve(newData, newMap)
    }

    _solve((sPrime zip frequencies).toMap, Map(0L -> Nil)).sorted.mkString(" ")
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
          itr.next()
          val seq = Seq(itr.next(), itr.next())
          println(f"Case #$set: ${solve(seq)}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}