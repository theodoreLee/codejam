import java.io.FileOutputStream

import scala.io.Source

object KingdomRush {
  val INPUT = "B-large-practice.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"


  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val sets = itr.next().toInt

    val writer = new FileOutputStream(OUTPUT)
    //    val writer = Console.out
    try {
      Console.withOut(writer) {
        for (set <- 1 to sets) {
          val stages = itr.next().toInt
          val parse = for {
            stage <- (0 until stages).toList
          } yield itr.next().split(" ").map(_.toInt)
          println(f"Case #${set}: ${minTimes(parse, 0, 0)}")
        }
      }
    } finally {
      writer.close()
    }
  }

  def minTimes(stages: List[Array[Int]], stars: Int, times: Int): String = {
    if (stages == Nil) times.toString
    else {
      //find beatable lvl2 stages
      stages.partition(_(1) <= stars) match {
        case (sat, others) if (sat.length > 0) =>
          val earnedStars = sat.flatten.filter(_ > -1).length
          minTimes(others, stars + earnedStars, sat.length + times)

        case _ =>
          //find a beatable lvl1 stage.
          val item = stages.filter(x => x(0) != -1 && x(0) <= stars)
          if (item == Nil)  "Too Bad"
          else {
            val index = stages.indexOf(item.maxBy(_(1)))
            val xs = stages.updated(index, Array(-1, stages(index)(1)))
            minTimes(xs, stars + 1, times + 1)
          }
      }
    }
  }
}