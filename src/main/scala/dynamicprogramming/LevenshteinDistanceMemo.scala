package dynamicprogramming

/*
import scalaz.memo.Memo
import scalaz.memo.SizedMemo.arraySizedMemo

object Levenshtein {
  def levenshtein[A](n: Array[A], y: Array[A]): Int = {
    val im = arraySizedMemo
    val m = im[Memo[Int, Int]](n.length + 1)
    // the call matrix
    def mx(i: Int, j: Int): Int = if(i == 0) j else if(j == 0) i else {
      def f = (n: Int) => im[Int](y.length + 1)
      val a = m(f)(i - 1)(mx(i - 1, _))(j) + 1
      val b = m(f)(i - 1)(mx(i - 1, _))(j - 1) + (if(n(i - 1) == y(j - 1)) 0 else 1)
      lazy val c = m(f)(i)(mx(i, _))(j - 1) + 1
      if(a < b) a else if(b <= c) b else c
    }
    mx(n.length, y.length)
  }

  def main(args: Array[String]) =
    println(levenshtein(args(0).toCharArray, args(1).toCharArray))
}
*/