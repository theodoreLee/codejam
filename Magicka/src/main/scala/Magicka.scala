
import java.io.FileOutputStream

import scala.io.Source

object Magicka {
  val INPUT = "B-large-practice.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out2"


  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val sets = itr.next().toInt

    val writer = new FileOutputStream(OUTPUT)
//        val writer = Console.out
    try {
      Console.withOut(writer) {
        for (set <- 1 to sets) {
          val problem = itr.next().split(' ').toList
          val (list, combine) = combines(problem.head.toInt, problem.tail, Nil)
          val (others, opposedStuff) = findOpposedChars(list.head.toInt, list.tail, Nil)
          val elements = others.tail.head.toCharArray
          println(f"Case #${set}: ${solve(elements, combine, opposedStuff).mkString("[",", ","]")}")
        }
      }
    } finally {
      writer.close()
      System.exit(0)
    }
  }

  def combines(total:Int, list:List[String], accr:List[(Set[Char],Char)]):(List[String],List[(Set[Char],Char)]) = (total, list) match {
    case (0, _) => (list,accr)
    case (_, x::xs) => combines(total - 1, xs, (Set[Char](x(0),x(1)),x(2)) :: accr)
  }

  def findOpposedChars(total:Int, list:List[String], accr:List[Set[Char]]):(List[String],List[Set[Char]]) = (total, list) match {
    case (0, _) => (list,accr)
    case (_, x::xs) => findOpposedChars(total - 1, xs, Set(x(0),x(1)) :: accr)
  }


  def solve(elements:Array[Char], c:List[(Set[Char],Char)], op:List[Set[Char]]):Array[Char]  = {
    def _solve(elem:Array[Char], o:List[(Char,Int)], pos:Int):Array[Char] = elem match {
      case _ if elem.length  <= pos => elem
      case _ if pos > 0 && c.exists(_._1 == Set(elem(pos),elem(pos-1))) =>
        val (_, t) = c.find(_._1 == Set(elem(pos),elem(pos-1))).get
        elem.update(pos-1,t)
        val (head, tail) = elem.splitAt(pos)
        val newArray = if(tail.length > 0) head ++ tail.tail else head
        _solve(newArray, o.filterNot( _._2 >= pos-1), pos)
      case _ if o.exists(elem(pos) == _._1) => _solve(elem.drop(pos+1), Nil, 0)
      case _ =>
        val os = op.filter(_.exists(_ == elem(pos)))
          .map(s => if(s.head == elem(pos)) (s.tail.head,pos) else (s.head,pos))
          .filterNot(x => o.exists(x._1 == _._1))
        _solve(elem, o ++ os, pos+1)
    }
    _solve(elements,Nil, 0)
  }
}


