import java.io.PrintStream
import scala.io.Source
import scala.collection.mutable.PriorityQueue

object NewTravel {

  def solve(itr: Iterator[String]) = {
    val Array(cities, loads, tests) = itr.next().split(' ').map(_.toInt)

    def createMap(i: Int, costMap: Map[(Int, Int), Array[Int]], map: Map[Int, Vector[Int]]): (Map[(Int, Int), Array[Int]], Map[Int, Vector[Int]]) = i match {
      case -1 => (costMap, map)
      case _ =>
        val Array(from, to) = itr.next().split(' ').map(_.toInt)
        val arr = itr.next().split(' ').map(_.toInt)
        val connectedWithFrom = map.getOrElse(from, Vector()) :+ to
        val connectedWithTo = map.getOrElse(to, Vector()) :+ from
        createMap(i - 1, costMap + ((from, to) -> arr) + ((to, from) -> arr), map + (from -> connectedWithFrom) + (to -> connectedWithTo))
    }

    val (costMap, map) = createMap(loads - 1, Map(), Map())

    def findMin(queue:PriorityQueue[(Int,Int)], dest:Int, set:Set[Int]):Option[Int] = {
      if(queue.isEmpty) None
      else {
        val (node, hour) = queue.dequeue()
        if(set.contains(node)) {
          findMin(queue, dest, set)
        } else {
          if(node == dest) Some(hour)
          else {
            val traversableList = map.getOrElse(node, Vector()).filterNot(set.contains).map { d =>
              val cost = costMap((node, d))(hour%24)
              (d, hour + cost)
            }
            findMin(queue ++ traversableList, dest, set + node)
          }
        }
      }
    }
    (0 until tests).map { i =>
      val Array(d, time) = itr.next().split(' ').map(_.toInt)
      println("*" * 20)
      val queue = new PriorityQueue[(Int,Int)]()(Ordering.fromLessThan(_._2 > _._2))
      queue += ((1,time))
      findMin(queue, d, Set()).map(_ - time).getOrElse(-1)
    }.mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out2"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve(itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}