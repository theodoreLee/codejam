import java.io.PrintStream

import scala.collection.mutable
import scala.io.Source

case class Time(head: Int, dur: Int, w: Int) extends Ordered[Time] {
  override def compare(that: Time): Int = that.dur
}

object Main {

  def count(a: IndexedSeq[(Int, Int, Int)]) = {
    println(a.mkString("-"))
    var cnt = a.length
    for {
      i <- 1 until a.length
      if a(i - 1)._3 == a(i)._3
    } {
      cnt += 1
    }
    if (a(a.length - 1)._3 == a(0)._3) cnt += 1
    cnt
  }

  def solve(set:Int, itr: Iterator[String]) = {
    println("=" * 50)
    val Array(ac, aj) = itr.next().split(' ').map(_.toInt)
    val cameron = (0 until ac).map(_ => {
      val Array(c, d) = itr.next().split(' ').map(_.toInt)
      (c, d, 1)
    })
    val Jamie = (0 until aj).map(_ => {
      val Array(c, d) = itr.next().split(' ').map(_.toInt)
      (c, d, 0)
    })
    val arr = Array(Jamie.map(a => a._2 - a._1).sum, cameron.map(a => a._2 - a._1).sum)
    var activities = (cameron ++ Jamie).sortBy(_._1)
    if(set == 5) {
      println(arr.mkString(","))
      println(activities.mkString("-"))
    }
    if (arr.max != 720) {
      val pq: mutable.PriorityQueue[Time] = mutable.PriorityQueue()
      for {
        i <- 1 until activities.length
      } {
        val before = activities(i - 1)
        val cu = activities(i)
        if (before._3 == cu._3) pq.enqueue(Time(before._1, cu._1 - before._2, before._3))
      }

      if (activities.length > 2 && activities(activities.length - 1)._3 == activities(0)._3) {
        pq.enqueue(Time(activities(activities.length - 1)._1, 1440 - activities(activities.length - 1)._2 - activities(0)._1, activities(0)._3))
      }
      var length = activities.length
      while (pq.nonEmpty) {
        val item = pq.dequeue()
        if (arr(item.w) != 720 || item.dur != 0) {
          if (720 < arr(item.w) + item.dur) {
            println(item.w, item.dur)
            arr(item.w) = 720
          }
          else {
            val idx = activities.indexWhere(_._1 == item.head)
            if (idx > -1) {
              arr(item.w) = arr(item.w) + item.dur
              activities = activities.slice(0, idx) ++ activities.slice(idx + 1, activities.length)
            }
          }
        }
      }
      println(arr.mkString(","))
    }
    count(activities)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-small-attempt0.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve(set, itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}