
import java.io.FileOutputStream

import scala.io.Source

object SwingingWild {
  val INPUT = "A-large-practice.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"

  //A의 최대 반지름이 늘어나도, A에서 갈수 있는 덩굴의 수만 늘어날 뿐, A->B 덩쿨의 반지름은 늘지 않는다.
  case class Vine(pos: Int, length: Int) {
    private var maxRadius = 0

    def availableVines(list: List[Vine]): List[Vine] = list.takeWhile(x => x.pos <= pos + maxRadius)

    def setRadius(before: Vine) {
      maxRadius = Math.max(maxRadius, Math.min(pos - before.pos, length))
    }

    def touch(list: List[Vine], goal: Int): Boolean = {
      if (pos + maxRadius < goal) {
        availableVines(list).foreach(_.setRadius(this))
        false
      } else true
    }
  }


  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val sets = itr.next().toInt

    val writer = new FileOutputStream(OUTPUT)
    //    val writer = Console.out
    try {
      Console.withOut(writer) {
        for (set <- 1 to sets) {
          val vines = itr.next().toInt
          val parse = for {
            row <- (0 until vines).toList
          } yield {
              val line = itr.next().split(' ').map(_.toInt)
              Vine(line(0), line(1))
            }
          val maxDistance = itr.next().toInt
          println(f"Case #${set}: ${solve(parse, maxDistance)}")
        }
      }
    } finally {
      writer.close()
    }
  }


  def solve(list: List[Vine], maxDistance: Int): String = {
    list.head.setRadius(Vine(0, 0))

    def explore(list: List[Vine]): String = list match {
      case Nil => "NO"
      case x :: xs =>
        if (x.touch(xs, maxDistance)) "YES"
        else explore(xs)
    }
    explore(list)
  }
}

