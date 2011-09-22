package dynamicprogramming

object LevenshteinDistance {

  def calcLevenshteinDistance(s : String, t : String) : Int = {

    val m = s.length();
    val n = t.length();
    var d = new Array[Array[Int]](m+1, n+1);

    for (i <- 0 to m) {
      d(i)(0) = i
    }

    for (j <- 0 to n) {
      d(0)(j) = j;
    }

    var cost = 0;
    for (j <- 1 to n) {
      for (i <- 1 to m) {
        if (s.charAt(i-1) == t.charAt(j-1)) {
          cost = 0
        }
        else {
          cost = 1
        }
        d(i)(j) = math.min(
            math.min(d(i-1)(j) + 1, d(i)(j-1) + 1),
            d(i-1)(j-1) + cost);
      }
    }

    d(m)(n);
  }

  def main(args: Array[String]) = {

    println("Levenshtein Distance")
    val s1 = "kitten"
    val t1 = "sitting"
    val d1 = calcLevenshteinDistance(s1, t1)
    println("Distance = " + d1)

    val s2 = "Saturday"
    val t2 = "Sunday"
    val d2 = calcLevenshteinDistance(s2, t2)
    println("Distance = " + d2)
  }
}
