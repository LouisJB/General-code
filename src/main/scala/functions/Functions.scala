package functions

object Functions {

  // mimic F# << operator

  // using structural typing
  //implicit def to_<<[T, R](f: Function1[T, R]) = new { def <<[S](g: Function1[R, S]) = f andThen g }

  implicit def toFunction1W[T, R](f: Function1[T, R]) = new Function1W(f)
  implicit def toAnyW[T](a: T) = new AnyRefW(a)

  def main(args :Array[String]) {

    val double = (n : Int) => n * 2
    val square = (n : Int) => n * n

    // <<
    val f1 = double.andThen(square)
    val f2 = double << square
    
    val r1 = f1(22)
    val r2 = f2(22)
    val r3 = square(double(22))

    println(r1 + ", " + r2 + ", " + r3)

    // >>
    val f3 = square.andThen(double)
    val f4 = double >> square

    val r4 = f3(22)
    val r5 = f4(22)
    val r6 = double(square(22))

    println(r4 + ", " + r5 + ", " + r6)
    

    // |>
    val r7 = 22 |> double

    println(r7)
  }
}

// Pimped Function1 class with F# style operators
class Function1W[T, R](f: Function1[T, R]) {
  def <<[S](g: Function1[R, S]) = f andThen g
  def >>[S](g: Function1[S, T]) = g andThen f
}

class AnyRefW[T](arg: T) {
  def |>[R](f: Function1[T, R]) = f(arg)
  //def <|[T, R](f: Function1[T, R]) = f(arg)
}
