import scala.collection.mutable

case class P(hd: Int, ad: Int, hk: Int, ak: Int)

object Dragon {
  def main(args: Array[String]): Unit = {
    val in = io.Source.stdin.getLines()
    val T = in.next().toInt
    for (cs <- 1 to T) {
      val Array(ohd, ad, hk, ak, b, d) = in.next().split(" ").map(_.toInt)

      case class State(hd: Int, ad: Int, hk: Int, ak: Int, n: Int) extends Ordered[State] {
        override def compare(that: State): Int = that.n.compare(n)
        def pos = P(hd, ad, hk, ak)

        def attack: State = { val nh = hk - ad; State(if (nh > 0) hd - ak else hd, ad, nh, ak, n + 1) }
        def buff: State = { State(hd - ak, ad + b, hk, ak, n + 1) }
        def cure: State = { State(ohd - ak, ad, hk, ak, n + 1) }
        def debuff: State = { val na = Math.max(0, ak - d); State(hd - na, ad, hk, na, n + 1) }
      }

      val start = State(ohd, ad, hk, ak, 0)
      val seen = mutable.Set[P](start.pos)
      val q = mutable.PriorityQueue(start)
      var steps = -1
      def doTry(s: State): Unit = {
        if (s.hd > 0 && seen.add(s.pos)) q.enqueue(s)
      }
      while (q.nonEmpty) {
        val s = q.dequeue()
        if (s.hk <= 0) {
          steps = s.n
          q.clear()
        } else {
          doTry(s.attack)
          doTry(s.buff)
          doTry(s.cure)
          doTry(s.debuff)
        }
      }

      if (steps == -1) {
        println(s"Case #$cs: IMPOSSIBLE")
      } else {
        println(s"Case #$cs: $steps")
      }
    }
  }
}
