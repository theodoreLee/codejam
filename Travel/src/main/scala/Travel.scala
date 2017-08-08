import java.io.PrintStream
import scala.io.Source

object Travel {

  case class Path(currentPos: Int, time: Int, cost: Int, paths: Set[Int])

  def solve(itr: Iterator[String]) = {
    println("-" * 50)
    val Array(cities, loads, tests) = itr.next().split(' ').map(_.toInt)

    def createMap(i: Int, costMap: Map[(Int, Int), Array[Int]], map: Map[Int, Vector[Int]]): (Map[(Int, Int), Array[Int]], Map[Int, Vector[Int]]) = i match {
      case -1 => (costMap, map)
      case _ =>
        val Array(from, to) = itr.next().split(' ').map(_.toInt)

        val arr = itr.next().split(' ').map(_.toInt)

        val fValue = map.getOrElse(from, Vector()) :+ to
        val tValue = map.getOrElse(to, Vector()) :+ from
        createMap(i - 1, costMap + ((from, to) -> arr) + ((to, from) -> arr), map + (from -> fValue) + (to -> tValue))
    }

    val (costMap, map) = createMap(loads - 1, Map(), Map())

    def findPath(accr: Vector[Path], dest: Int, ret: Int): Int = accr match {
      case Vector() => ret
      case x +: xs =>
        val available = map.getOrElse(x.currentPos, Vector()).filterNot(i => x.paths.contains(i))

        if (available.isEmpty) {
          findPath(xs, dest, ret)
        }
        else {
          val (cPath, newPaths) = available.map { path =>
            val cost = costMap((x.currentPos, path))(x.time)
            Path(path, (x.time + cost) % 24, x.cost + cost, x.paths + path)
          }.partition(_.currentPos == dest)
          val newPath = if (ret != -1) (xs ++ newPaths).filter(_.cost < ret) else xs ++ newPaths
          val newRet = cPath.headOption match {
            case Some(r) if ret == -1 => r.cost
            case Some(r) => r.cost min ret
            case None => ret
          }
          findPath(newPath, dest, newRet)
        }
    }

    (0 until tests).map { i =>
      val Array(d, time) = itr.next().split(' ').map(_.toInt)
      println("*" * 20)
      val ret = findPath(Vector(Path(1, time, 0, Set(1))), d, -1)
      ret
    }.mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
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