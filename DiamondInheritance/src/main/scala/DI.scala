import java.io.FileOutputStream
import scala.io.Source

object DI {
  val INPUT = "A-large-practice.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out3"
  val isConsole = false

  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new FileOutputStream(OUTPUT)
    try {
      Console.withOut(stream) {
        val sets = itr.next().toInt
        (1 to sets).foreach { set =>
          val classes = itr.next().toInt
          val lists = for {
            i <- 1 to classes
          } yield {
              itr.next().split(' ').map(_.toInt).tail.toVector
          }
          println(f"Case #$set: ${hasDI(lists.toVector)}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }

  def hasDI(list:Vector[Vector[Int]]) = {
    def solve(l:Vector[Int],set:Set[Int]):Boolean = l match {
      case Vector() => false
      case x +: xs if set contains x => true
      case x +: xs => solve(xs ++ list(x-1), set + x)
    }

    list.filter(_.length > 1).exists(solve(_, Set())) match {
      case true => "Yes"
      case _ => "No"
    }
  }
}
