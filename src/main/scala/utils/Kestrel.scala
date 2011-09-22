package utils

object Kestrel {

  def main(args : Array[String]) {

    val r = newInstance[Foo](Array[AnyRef]()) { i =>

      i.x = 7
    }

    println("" + r.x)
  }

  def newInstance[T](args: Array[AnyRef])(op: T => Unit)(implicit m: Manifest[T]): T = {
    val constructor =
      m.erasure.getDeclaredConstructors.head
    val v = constructor.newInstance(args: _*).asInstanceOf[T]
    op(v)
    v
  }

  class Foo() {

    var x = 0;
  }
}
