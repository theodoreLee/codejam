//import java.io.FileOutputStream
//
//import scala.collection.SortedMap
//import scala.io.Source
//
///**
// * Created by teddy on 5/4/15.
// */
//object NoisyNeighbors {
//  val INPUT = "B-small-practice.in"
//  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
//
//
//  def main(args: Array[String]): Unit = {
//    val itr = Source.fromFile(INPUT).getLines()
//    val sets = itr.next().toInt
//
//    val writer = new FileOutputStream(OUTPUT)
////    val writer = Console.out
//    try {
//      Console.withOut(writer) {
//        for (set <- 1 to sets) {
//          val Array(r,c,n) = itr.next().split(" ").map(_.toInt)
//          println(f"Case #${set}: ${minimumUnhappiness(r,c,n)}")
//        }
//      }
//    } finally {
//      writer.close()
//      System.exit(0)
//    }
//  }
//
//  def minimumUnhappiness(R:Int, C:Int, N:Int):Int = {
//    val array = Array.tabulate[Int](R, C) {(i,j)=>
//      var n = 4
//      if(i == 0 || i + 1 == R) n -= 1
//      if(j == 0 || j + 1 == C) n -= 1
//      n
//    }
//
//    def set(n:Int, r:Int, c:Int):Int ={
//      n match {
//        case 0 => 0
//        case _ if r >= R =>n
//        case _ if c >= C => set(n, r + 1, (c + 1) % 2)
////        case _ if check =>
////          array(r).update(c, 0)
////          set(n-1, r, c + 2, unhappiness + findUnhappiness(r,c), check)
//        case _ =>
//          array(r).update(c, 0)
//          set(n -1, r, c+2)
//      }
//    }
//
//    def findUnhappiness(r:Int, c:Int) = {
//      var n :Int = 1 //own
//      //항상 좌에서 우 아래에서 위로.
//      if(r > 0 && array(r-1)(c) == 1) {
//        array(r-1).update(c, 0)
//        n += 1
//      }
//
//      if(c > 0 && array(r)(c-1) == 1) {
//        array(r).update(c-1, 0)
//        n += 1
//      }
//      if(r + 1 < R && array(r+1)(c) == 1) {
//        array(r+1).update(c , 0)
//        n += 1
//      }
//      if(c + 1 < C && array(r)(c+1) == 1) {
//        array(r).update(c+1, 0)
//        n += 1
//
//      }
//      n
//    }
//    def minus(idx:Int, fArray:Array[(Int,Int,Int)]) = {
//      val (x,y,z) = fArray(idx)
//      fArray.update(idx, (x,y, z -1))
//    }
//    if(R*C > 2 && (N == R*C) || (N + 1 == R*C)) N
//    else
//    set(N, 0, 0) match {
//      case 0 => 0
//      case n =>
//        val fArray = array.zipWithIndex.flatMap { case (arr, i) =>
//          arr.zipWithIndex.map(x=> (i,x._2, x._1))
//        }
//        val sortedMap = SortedMap
//
//        (0 until n).foreach{ x =>
//          val (x,y,z) = fArray.minBy(x=> if(x._3 == 0) 999 else x._3)
//          fArray.update((x*R)+y, (x,y,0))
////          if(x + 1 != R) minus((x+1)*R + y, fArray)
//          if(x + 2 != R) minus((x+2)*R + y, fArray)
//          if(y + 2 != C) minus(x*R+y+2, fArray)
//          if(x - 2 >= 0 )minus((x-2)*R + y, fArray)
//          if(y - 2 >= 0 )minus(x*R+y-2, fArray)
//        }
//        fArray.filter(_._3 != 0).length
//    }
//  }
//}

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
          val Array(r, c, n) = itr.next().split(' ').map(_.toInt)
          println(f"Case #$set: ${minimumUnhappiness2(r, c, n)}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }

  case class Value(x: Int, y: Int, private var _value: Option[Int]) {
    def value = _value

    def minus = _value = _value.map(_ - 1)

    var unhappiness = false

    def occupy(map: Map[(Int, Int), Value]) = {
      _value = None
      if (!unhappiness) {
        unhappiness = true
        map((x - 1, y)).minus
        map(x, y - 1).minus
        map(x + 1, y).minus
        map(x, y + 1).minus
      }
    }

    def notice(map: Map[(Int, Int), Value]) = {
      _value = None
      unhappiness = true
      map((x - 1, y)).occupy(map)
      map(x, y - 1).occupy(map)
      map(x + 1, y).occupy(map)
      map(x, y + 1).occupy(map)
    }
    def update {_value=None}
  }


//  def set(array: Array[Array[Int]]) = {
//    array.flatten.sortBy(x => if (x == -1) 999 else x)
//  }

  def toMap[T](array: Array[Array[T]]): Map[(Int, Int), T] = {
    (for {
      i <- array.indices
      j <- array(i).indices
    } yield (i, j) -> array(i)(j)).toMap
  }
  def printMap(array:Array[Array[Option[Int]]]) {
    println("=" * 50)
    println(array.map(_.map {_.map(_.toString).getOrElse("X")}.mkString(",")).mkString("\n"))
  }

  def minimumUnhappiness2(R: Int, C: Int, N: Int): Int = {

    if ((R * C + 1) / 2 >= N) 0
    else {
      val emptyRooms = R * C - N
      val walls = 2 * R * C - R - C //R * (C - 1) + C * (R -1)
      val centerRooms = ((R - 2) * (C - 2)) max 0
      if ((centerRooms + 1) / 2 >= emptyRooms) walls - emptyRooms * 4
      else {
        var n = 0
        val outC = C - 1
        val outR = R - 1
        val array = Array.tabulate(R, C) {
          (r,c)=>Value(r,c,Some(Vector(r, c, outR - r, outC - c).count(_ > 0)))
//          Value(r,c,Some(Vector(r, c, outR - r, outC - c).count(_ == 1)))

//          case (r, c) if r > 0 && c > 0 && c < outC && r < outR =>
//            if ((r + c) % 2 == 0) {
//              n += 1
//              Value(r,c,None)
//            }
//            else {
//              Value(r,c,Some(Vector(r, c, outR - r, outC - c).count(_ == 1)))
//            }
//          case (r,c) if (r+c)%2 == 0 =>
//            val res = Vector(r, c, outR - r, outC - c)
//            Value(r,c,Some(res.count(_ > 0)))
//          case (r,c) =>
//            val v = List(r, c, outR - r, outC - c).count(_ > 0) match {
//              case 3 if R != 2 && C != 2 => Some(2)
//              case x => Some(x)
//            }
//            Value(r,c,v)
        }
        val map = toMap(array).withDefaultValue(Value(-1, -1, None))

        def set(n: Int, rooms: Vector[Value]): Unit = n match {
          case 0 => Unit
          case _ =>
            val sortedRooms = rooms.sortBy(_.value).reverse
            sortedRooms.head.occupy(map)
//            val (x, y) = (targetRoom.x, targetRoom.y)
//            targetRoom.notice(map)
            //            printMap
            set(n - 1, sortedRooms.tail)
        }
//        println(emptyRooms - n)
        printMap
        set(emptyRooms - n, map.values.filter(_.value.isDefined).toVector.sortBy(_.value).reverse)
        def printMap {
          println("=" * 50)
          println(array.map(_.map(_.value.map(_.toString).getOrElse("X")).mkString(",")).mkString("\n"))
//          println(array.map(_.map {
//            case v => v.unhappiness match {
//              case true => "!"
//              case false =>
//                v.value.map(_.toString).getOrElse("X")
//            }
//          }.mkString(",")).mkString("\n"))
        }
        printMap
        val ret = (for {
          r <- 0 until R
          c <- 0 until C
        } yield
          map((r,c)) match {
            case Value(x,y,None)=> 0
            case Value(x,y,Some(_))=>
              List(map(x +1, y).value.isDefined, map(x, y+1).value.isDefined).count(_ == true)
//              List(R - x, C - y).count(_ != 1)
          }
        )
        ret.sum
      }
    }
  }

  def minimumUnhappiness(R: Int, C: Int, N: Int): Int = {
    if ((R * C)  / 2 >= N) 0
    else {
      val emptyRooms = R * C - N
      val walls = 2 * R * C - R - C //R * (C - 1) + C * (R -1)
      val centerRooms = ((R - 2) * (C - 2)) max 0
      if ((centerRooms + 1) / 2 >= emptyRooms) walls - emptyRooms * 4
      else {
        var n = 0
        val array = Array.tabulate(R, C) {
          (x, y) =>
            if ((x + y) % 2 == 0) {
              n += 1
              Value(x, y, None)
            }
            else {
              val res = Vector((x - 1), (y - 1), (R - x - 2), (C - y - 2))
              Value(x, y, Some(res.filter(_ > -1).length))
            }
        }
        val map = toMap(array).withDefaultValue(Value(-1, -1, None))
        //        printMap
        def set(n: Int, sortedRooms: Vector[Value]): Unit = n match {
          case 0 => Unit
          case _ if n <= 0 => Unit
          case _ =>
            //          val sortedRooms = rooms.sortBy(_.value)
            val targetRoom = sortedRooms.head
            val (x, y) = (targetRoom.x, targetRoom.y)

            targetRoom.notice(map)
            //            printMap
            set(n - 1, sortedRooms.tail)
        }

        def printMap {
          println("=" * 50)
          println(array.map(_.map {
            case v => v.unhappiness match {
              case true => "!"
              case false =>
                v.value.map(_.toString).getOrElse("X")
            }
          }.mkString(",")).mkString("\n"))
        }

        set(N - n, map.values.filter(_.value.isDefined).toVector.sortBy(_.value))

        val walls = 2 * R * C - R - C //R * (C - 1) + C * (R -1)
        val _wall = (for {
            x <- array.indices
            y <- array(x).indices
            if map(x, y).value.isDefined
          } yield {
              val res = Vector((x - 1), (y - 1), (R - x - 2), (C - y - 2))
              res.filter(_ > -1).length
            }).sum
        walls - _wall
      }

    }

  }
}
