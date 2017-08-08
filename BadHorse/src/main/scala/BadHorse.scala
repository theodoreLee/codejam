import java.io.PrintStream
import scala.io.Source

object BadHorse {
  case class League(teamA:Set[String], teamB:Set[String]) {
    def parse(a:String, b:String) = {
      if(teamA.contains(a) && teamA.contains(b)) return false
      if(teamB.contains(a) && teamB.contains(b)) return false

      if(teamA.contains(a) || teamB.contains(a)) {

      }
    }
  }

  def solve(games:Int, seq: Iterator[String]) = {
    def parse(a:String, b:String) = {

    }
    def findSolution(leagues:Vector[League],)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "test.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        val games = itr.next().toInt
        stream.println(f"Case #$set: ${solve(games, itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}