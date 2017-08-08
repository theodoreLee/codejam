import java.io.FileOutputStream

import scala.collection.immutable.SortedMap
import scala.io.Source

object HikingDeer {
  val INPUT = "C-large-practice.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
  val isConsole = false

  case class Hiker(speed: Int, counter: Int = 0)

  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new FileOutputStream(OUTPUT)
    try {
      Console.withOut(stream) {
        val sets = itr.next().toInt
        (1 to sets).foreach { set =>
          val inputs = for {
            hikers <- 1 to itr.next().toInt
          } yield {
              val Array(start, numberOfHikers, speed) = itr.next().split(' ').map(_.toInt)
              (start, numberOfHikers, speed)
            }
          val map = sortedMap(inputs)

          println(f"Case #$set: ${minEncounter(map)}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }

  def sortedMap(seq: Seq[(Int, Int, Int)]): SortedMap[Double, Seq[Hiker]] = {
    (SortedMap[Double, Seq[Hiker]]() /: seq) {
      case (map, (start, numberOfHikers, initSpeed)) =>
        val list = for {
          hiker <- 0 until numberOfHikers
          speed = initSpeed + hiker
        } yield {
            val time = (360 - start.toDouble) / 360 * speed
            (time, Hiker(speed, 0))
          }
        (map /: list) {
          case (map, (time, hiker)) =>
            val values = map.getOrElse(time, Seq())
            map + (time -> (hiker +: values))
        }
    }
  }

  def minEncounter(map: SortedMap[Double, Seq[Hiker]]) = {
    val h = map.values.map(_.size).sum
    val `2h` = 2 * h
    def solve(sortedMap: SortedMap[Double, Seq[Hiker]], current: Int, min: Int): Int = {
      if (min == 0 || current >= `2h`) min
      else {
        val (time, hikers) = sortedMap.head
        val d = hikers.map(hiker => if (hiker.counter == 0) -1 else 1).sum
        val newMap = (sortedMap.tail /: hikers) {
          case (sm, hiker) =>
            val newTime = time + hiker.speed
            val values = sm.getOrElse(newTime, Seq())
            sm + (newTime -> (hiker.copy(counter = hiker.counter + 1) +: values))
        }
        solve(newMap, current + d, min min (current + d))
      }
    }
    solve(map, h, h)
  }
}