
import java.io.FileOutputStream

import scala.io.Source

object CrazyRows {
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
          val matrixSize = itr.next().toInt
          val parse = for {
            row <- (0 until matrixSize).toList
          } yield itr.next().lastIndexOf("1")
          println(f"Case #${set}: ${solve(parse)}")
        }
      }
    } finally {
      writer.close()
    }
  }

  def solve(list: List[Int], row: Int = 0): Int = list match {
    case Nil => 0
    case x :: xs if x <= row => solve(list.tail, row + 1)
    case _ =>
      val idx = list.indexWhere(_ <= row)
      val (front, back) = list.splitAt(idx)
      solve(front ++ back.tail, row + 1) + idx
  }
}


