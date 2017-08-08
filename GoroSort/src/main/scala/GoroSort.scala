import java.io.FileOutputStream

import scala.io.Source

object GoroSort {
  val INPUT = "D-large-practice.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"


  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val sets = itr.next().toInt

    val writer = new FileOutputStream(OUTPUT)
//    val writer = Console.out
    try {
      Console.withOut(writer) {
        for (set <- 1 to sets) {
          itr.next()
          val array = itr.next().split(' ').map(_.toInt).toList
          val ret = array.foldLeft((1,0)){case ((pos,y),z)=> if(pos == z) (pos+1, y) else (pos+1,y+1)}
//          println(f"Case #${set}: ${sort(array)}%.6f")
          println(f"Case #${set}: ${ret._2}%.6f")
        }
      }
    } finally {
      writer.close()
      System.exit(0)
    }
  }

  def sort(array:List[Int], i:Int = 1, cnt:Int = 0):Int = array match {
    case Nil =>  cnt
    case x :: xs if(x == i) => sort(xs, i+1, cnt)
    case x :: xs =>
      val idx = xs.indexOf(i)
      sort(xs.updated(idx,x), i + 1, cnt + 2)
  }
}