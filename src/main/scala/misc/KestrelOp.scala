package misc

object KestrelOp {


  // check also FJ db
  // def closeAfter[A](f: InputStream => A)(a: InputStream) = try { throws(f(a)) } finally { in.close } // for example + correction
  //
  def main(args : Array[String]) {

    println("result = " + func(1))
  }

  def func(n : Int) = {

    kestrel {
      new Foo(5 + n)
    } {
      x => println("inner" + x)
    }
  }

  def kestrel[T](bodyFn : => T)(resultFn : T => Unit) : T = {
    val result = bodyFn
    resultFn(result)
    result
  }
}

class Foo(n : Int) { override def toString() = "{ n = " + n + "}" }
