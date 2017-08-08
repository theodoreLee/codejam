import java.io.PrintStream
import scala.io.Source

object DynamicGridSet {
  def dropMass(accr:Vector[(Int,Int)], restSet:Set[(Int,Int)]):Set[(Int,Int)]  = accr match {
    case Vector() => restSet
    case x +: xs if restSet.contains(x) =>
      dropMass(xs :+ (x._1 + 1, x._2) :+ (x._1 - 1, x._2) :+ (x._1, x._2 +1) :+ (x._1, x._2 -1), restSet - x)
    case x +: xs =>
      dropMass(xs, restSet)
  }

  def findMass(set:Set[(Int,Int)], accr:Int):Int = set match {
    case _ if set.isEmpty => accr
    case s => findMass(dropMass(Vector(s.head), s), accr + 1)
  }

  def solve(itr: Iterator[String])(stream:PrintStream) {
    val Array(r, _) = itr.next().split(' ').map(_.toInt)
    val grid = (0 until r).flatMap(i => itr.next().map(_ == '1').zipWithIndex.filter(_._1).map(x => (i, x._2))).toSet
    val cases = itr.next().toInt
    (grid /: (0 until cases)) { (set,_) =>
      itr.next().split(' ') match {
        case Array("Q") =>
          stream.println(findMass(set, 0))
          set
        case Array("M", rString, cString, "1") => set + ((rString.toInt, cString.toInt))
        case Array("M", rString, cString, "0") => set - ((rString.toInt, cString.toInt))
      }
    }
    println("done...")
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
        stream.println(f"Case #$set:")
        solve(itr)(stream)
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}