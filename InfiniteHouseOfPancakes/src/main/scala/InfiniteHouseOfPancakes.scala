import java.io.FileOutputStream

import scala.io.Source
import scala.math._
/**
 * Input
 *
 * The first line of the input gives the number of test cases, T.
 * T test cases follow. Each consists of one line with D, the number of diners with non-empty plates,
 * followed by another line with D space-separated integers representing the numbers of pancakes on those diners' plates.
 *
 * Output
 *
 * For each test case, output one line containing "Case #x: y",
 * where x is the test case number (starting from 1) and
 * y is the smallest number of minutes needed to finish the breakfast.
 */
object InfiniteHouseOfPancakes {
//  val INPUT = "B-small-attempt1.in"
  val INPUT = "test.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"


  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val sets = itr.next().toInt

//    val writer = new FileOutputStream(OUTPUT)
    val writer = Console.out
    try {
      Console.withOut(writer) {
        for (set <- 1 to sets) {
          itr.next()
          val sortedList = itr.next().split(' ').map(_.toInt).sortWith(_ > _)
          println(s"Case #${set}: ${findOpticalMinutes(sortedList)}")
        }
      }
    } finally {
      writer.flush()
      writer.close()
    }
  }


  def findOpticalMinutes(list:IndexedSeq[Int], minutes:Int = 0):Int = list match {
    case _ if(list.isEmpty || list.head == 1) => minutes + 1
    case x +: xs =>
      val length = xs.takeWhile(_ == x).length + 1
      val left = round(x / 2.0).toInt
      val take = x - left
      val total = list.sum.toDouble
      val freeToEat = total / (list.length - length)
      val interrupt = total / (list.length + length)
      println(freeToEat, interrupt)
      if(freeToEat > interrupt) {
        println(list.mkString(","))
        println(freeToEat, interrupt)
        List(1,2) *
        IndexedSeq((left,take) * 3)
        findOpticalMinutes((left +: take +: xs).sortWith(_ > _), minutes +1)
      } else {
//        println("hold")
//        findOpticalMinutes(list.map(_ - 1).filterNot(_ == 0), minutes + 1)
        minutes + list.max
      }
  }

}
