import java.io.FileOutputStream
import scala.io.Source

object NoisyNeighbors {
  val INPUT = "test.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
  val isConsole = true

  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new FileOutputStream(OUTPUT)
    try {
      Console.withOut(stream) {
        val sets = itr.next().toInt
        (1 to sets).foreach { set =>
          val Array(r,c,n) = itr.next().split(' ').map(_.toInt)
          println(f"Case #$set: ${minimumUnhappiness(r,c,n)}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }

  case class Value(x:Int ,y:Int, private var _value:Option[Int]) {
    def value = _value
    def minus = value.map(_ - 1)
    def occupy(map:Map[(Int,Int),Value]) = {
      _value = None
      map((x -1, y)).minus
      map(x, y -1).minus
      map(x + 1, y).minus
      map(x, y +1).minus
    }
  }


  def set(array:Array[Array[Int]]) = {
    array.flatten.sortBy(x => if(x == -1) 999 else x)
  }

  def toMap(array:Array[Array[Value]]):Map[(Int,Int),Value] = {
    (for {
      i <- array.indices
      j <- array(i).indices
    } yield (i,j)-> array(i)(j)).toMap.withDefaultValue(Value(-1,-1,None))
  }

  def minimumUnhappiness(R:Int, C:Int, N:Int):Int = {
    if((R * C) / 2 >= N) 0
    else {
      val array = Array.tabulate(R,C) {
        (x, y) =>
          val res = Vector((x-1), (y-1), (R - x - 2), (C - y - 2))
          Value(x,y, Some(res.filter(_ > -1).length))
      }
      val map = toMap(array)

      def set(n:Int, rooms:Vector[Value]):Unit = n match {
        case 0 => Unit
        case _ =>
          val sortedRooms = rooms.sortBy(_.value)
          val targetRoom = sortedRooms.head
          targetRoom.occupy(map)
          set(n, sortedRooms.tail)
      }
      def isUnhappiness(x:Int, y:Int):Boolean = {
        if(map((x,y)).value.isDefined) false
        else {
          if(x != 0 && map((x-1,y)).value.isEmpty) return true
          if(y != 0 && map((x,y-1)).value.isEmpty) return true
          if(x + 1 != R && map((x +1, y)).value.isEmpty) return true
          if(y + 1 != C && map((x +1, y)).value.isEmpty) return true
          false
        }
      }
      set(N, map.values.toVector)

      (for {
        x <- array.indices
        y <- array(x).indices
      } yield {
          isUnhappiness(x,y)
        }).filter(_ == true).length
    }

  }
}
