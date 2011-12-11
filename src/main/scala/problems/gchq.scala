package problems


object GCHQ {

  def main(args : Array[String]) {

    val ls1 = List(
      "eb 04 af c2 bf a3 81 ec", // | 00 01 00 00 31 c9 88 0c",
      "0c fe c1 75 f9 31 c0 ba", // | ef be ad de 02 04 0c 00",
      "d0 c1 ca 08 8a 1c 0c 8a", // | 3c 04 88 1c 04 88 3c 0c",
      "fe c1 75 e8 e9 5c 00 00", // | 00 89 e3 81 c3 04 00 00",
      "00 5c 58 3d 41 41 41 41", // | 75 43 58 3d 42 42 42 42",
      "75 3b 5a 89 d1 89 e6 89", // | df 29 cf f3 a4 89 de 89",
      "d1 89 df 29 cf 31 c0 31", // | db 31 d2 fe c0 02 1c 06",
      "8a 14 06 8a 34 1e 88 34", // | 06 88 14 1e 00 f2 30 f6",
      "81 1c 16 8a 17 30 da 88", // | 17 47 49 75 de 31 db 89",
      "d8 fe c0 cd 80 90 90 e8"  // | 9d ff ff ff 41 41 41 41"
    )

    val ls2 = List(
      "00 01 00 00 31 c9 88 0c",
      "ef be ad de 02 04 0c 00",
      "3c 04 88 1c 04 88 3c 0c",
      "00 89 e3 81 c3 04 00 00",
      "75 43 58 3d 42 42 42 42",
      "df 29 cf f3 a4 89 de 89",
      "db 31 d2 fe c0 02 1c 06",
      "06 88 14 1e 00 f2 30 f6",
      "17 47 49 75 de 31 db 89",
      "9d ff ff ff 41 41 41 41"
    )

    val ls3 = ls1.zip(ls2).map(s => s._1 + s._2)
    val ls4 = ls1 ::: ls2

    def splitChars(ls : List[String]) = ls.flatMap(_.split(' '))

    val data1 = splitChars(ls1)
    val data2 = splitChars(ls2)
    val data3 = splitChars(ls3)
    val data4 = splitChars(ls4)


    basicFrequencies(data1)
    basicFrequencies(data2)
    basicFrequencies(data3)
    basicFrequencies(data4)

    def basicFrequencies(ls : List[String]) = {
      val g = ls.groupBy(x => x)

      println("Size = " + g.size)
      //g.foreach(println)

      println("--")
      val x = g.toList.sortWith(_._2.size > _._2.size)
      x.foreach(println)
    }
  }
}
