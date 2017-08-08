import java.io.PrintStream
import scala.io.Source

object DynamicGrid {

  def findSubset(array:Array[Boolean], c:Int, accr:Int):Int = {
    val idx = array.indexOf(true)
    if(idx == -1) accr
    else {
      def findMass(accr:Vector[Int]): Unit = accr match {
        case Vector() => Unit
        case x +: xs if x < 0 ||  x >= array.length || !array(x) => findMass(xs)
        case x +: xs if x > -1 &&  x < array.length && array(x) =>
          array(x) = false
          if((x + 1) % c == 0) findMass(xs :+ (x -1) :+ (x + c) :+ (x - c))
          else if(x % c == 0) findMass(xs :+ (x + 1) :+ (x + c) :+ (x - c))
          else findMass(xs :+ (x + 1) :+ (x -1) :+ (x + c) :+ (x - c))
      }
      findMass(Vector(idx))
      findSubset(array, c, accr + 1)
    }
  }

  def solve(itr: Iterator[String], stream:PrintStream):Unit = {
    val Array(r, c) = itr.next().split(' ').map(_.toInt)
    val grid = (0 until r).map(_ => itr.next().map(_ == '1')).toArray.flatten
    val cases = itr.next().toInt
    (0 until cases).foreach{_ =>
      val operation = itr.next().split(' ')
      operation match {
        case Array("Q") => stream.println(findSubset(grid.clone(), c, 0))
        case Array("M", rString, cString, value) =>
          val pos = rString.toInt * c + cString.toInt
          grid.update(rString.toInt * c + cString.toInt, value == "1")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-small-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set:")
        solve(itr, stream)
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}