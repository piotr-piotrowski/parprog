package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def check(chars: Array[Char], level: Int): Int =
      if (chars.isEmpty) level
      else if (chars.head == '(') check(chars.tail, level + 1)
      else if (chars.head == ')')
        if (level > 0)
          check(chars.tail, level - 1)//if (level > 0) level - 1 else 1)
        else -2
      else check(chars.tail, level)
    check(chars, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      def helper(chars: Array[Char], acc: (Int, Int)): (Int, Int) = {
        if (chars.isEmpty) acc
        else if (chars.head == '(') helper(chars.tail, (acc._1 + 1, acc._2))
        else if (chars.head == ')') {
          if (acc._1 > 0) helper(chars.tail, (acc._1 - 1, acc._2))
          else helper(chars.tail, (acc._1, acc._2 + 1))
        }
        else helper(chars.tail, acc)
      }
      helper(chars.slice(idx, until), (arg1, arg2))
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (from + until) / 2
        val (l, r) = parallel(reduce(from, mid), reduce(mid, until))
        val matched = scala.math.min(l._1, r._2)
        (l._1 + r._1 - matched, l._2 + r._2 - matched)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
