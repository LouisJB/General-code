package types

// http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/

object UnionTypes extends App {

  type ¬[A] = A => Nothing
  type ¬¬[A] = ¬[¬[A]]

  type ∨[T, U] = ¬[¬[T] with ¬[U]]

  implicitly[¬¬[Int] <:< (Int ∨ String)]
  implicitly[¬¬[String] <:< (Int ∨ String)]

  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

  def size[T : (Int |∨| String)#λ](t : T) = t match {
      case i : Int => i
      case s : String => s.length
  }

  println("size string " + size("hello"))
  println("size it " + size(12))
  //println("size it " + size(12.0))
}