import java.io.FileOutputStream
import scala.io.Source

object Energy {
  def solve(seq: Seq[String]):BigInt = {
    val Array(e,r,n) = seq.head.split(' ').map(_.toInt)
    val values = seq(1).split(' ').map(_.toInt).toVector

    case class Path(value:Int, currentEnerge:Int, paths:Vector[Int], traversable: Vector[Int]) {
      def nextPaths:Vector[Path] = {
        if(traversable.length == 1) {
          Vector(Path(value + traversable.head * currentEnerge, 0, paths :+ currentEnerge, Vector.empty))
        }  else {
          val minEnerge = 0 max (currentEnerge + r - e)
          for{
            p <- (minEnerge to currentEnerge).toVector
          } yield Path(value + p * traversable.head, currentEnerge - p + r, paths :+ p, traversable.tail)
        }
      }
    }

    def solveSmall(accr:Vector[Path], max:Path):Path = accr match {
      case Vector() => max
      case x +: xs if x.traversable.isEmpty => if(x.value > max.value) solveSmall(xs, x) else solveSmall(xs, max)
      case x +: xs => solveSmall(xs ++ x.nextPaths, max)
    }

    def solveLarge:BigInt = {
      val orderValues = values.zipWithIndex.sortWith(_._1 > _._1)
      val expectedEnergy = e + BigInt(r) * (n-1)

      val recoveryTurns = ((e-r) until 0 by -r).toList
      val checkSize = recoveryTurns.length

      def updateList(arr:Array[(Option[BigInt], BigInt)], range:Range):Array[(Option[BigInt],BigInt)] = {
        def _update(list:List[Int], i:Int, min:Int)  {
          if(i >= min) {
            arr(i) = (None, list.head)
            _update(list.tail, i - 1, min)
          }
        }
        if(!range.isEmpty) _update(recoveryTurns, range.max, range.min)
        arr
      }

      val ret = ((expectedEnergy, Array.fill[(Option[BigInt],BigInt)](values.size)((None, 0)), BigInt(0)) /: orderValues) {
        case ((remain, arr, total), (value, pos)) =>
          val range = (0 max pos - checkSize) until pos
          val alreadySet = range.filter(i => arr(i)._1.isDefined).toList
          alreadySet match {
            case Nil =>
              updateList(arr, range)
              val en = remain min (e - arr(pos)._2)
              arr(pos) = (Some(en), 0)
              (remain - en, arr, total + en * values(pos))
            case list =>
              val range = (list.max + 1) until pos
              updateList(arr, range)
              val en = (remain min ((pos - list.max) * r - arr(pos)._2)) max 0
              arr(pos) = (Some(en), 0)
              (remain - en, arr, total + en * values(pos))
          }
      }
      ret._3
    }

    solveLarge
  }


  def main(args: Array[String]): Unit = {
    val INPUT = "B-large-practice.in"
//    val INPUT ="test.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new FileOutputStream(OUTPUT)
    try {
      Console.withOut(stream) {
        val sets = itr.next().toInt
        (1 to sets).foreach { set =>
          val seq = Seq(itr.next(), itr.next())
          println(f"Case #$set: ${solve(seq)}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}