package projecteuler

object Problem17 {

  def main(args : Array[String]) {

    val oneTo19 : Int = (1 to 19).map(x => wordLenMap(x)).sum

    println("sum 1 to 19 = " + oneTo19)

    val oneTo99 : Int =
      oneTo19 +
      (20 to 99).map(x => wordLenMap((x / 10) * 10) + wordLenMap(x % 10)).sum

    println("sum 1 to 99 " + oneTo99)

    val oneHundredTo999 =
      (1 to 9).map(x => wordLenMap(x) + wordLenMap(100) + ((wordLenMap(x) + wordLenMap(100) + "and".size) * 99 + oneTo99)).sum

    val sum = oneTo99 + oneHundredTo999 + (wordLenMap(1) + wordLenMap(1000))

    println("total = " + sum)
  }

  val wordLenMap = Map[Int, Int](
    0 -> 0,
    1 -> 3,
    2 -> 3,
    3 -> 5,
    4 -> 4,
    5 -> 4,
    6 -> 3,
    7 -> 5,
    8 -> "eight".size,
    9 -> "nine".size,
    10 -> "ten".size,
    11 -> "eleven".size,
    12 -> "twelve".size,
    13 -> "thirteen".size,
    14 -> "fourteen".size,
    15 -> "fifteen".size,
    16 -> "sixteen".size,
    17 -> "seventeen".size,
    18 -> "eighteen".size,
    19 -> "nineteen".size,
    20 -> "twenty".size,
    30 -> "thirty".size,
    40 -> "forty".size,
    50 -> "fifty".size,
    60 -> "sixty".size,
    70 -> "seventy".size,
    80 -> "eighty".size,
    90 -> "ninety".size,
    100 -> "hundred".size,
    1000 -> "thousand".size
  )
}