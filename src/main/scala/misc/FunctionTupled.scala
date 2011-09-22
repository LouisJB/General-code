package misc

object FunctionTupled {

  def main(args : Array[String]) {
    
    def div(a : Int, b : Int) = (a / b).toString

    val r = f((4, 2))((div _).tupled)
    println(r)
  }

  def f[T, A <: String, B >: String](a : T)(ff : T => A) : B = {
    f1(a)(er, ff)
  }

  def f1[T, A](a : T)(e : Exception => A, ff : T => A) : A = {
    try {
      ff(a)
    }
    catch {
      case ex : Exception => e(ex)
    }
  }

  def er(ex : Exception) = ex.toString
}
