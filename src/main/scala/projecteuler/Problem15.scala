package projecteuler

object Problem15 {

  def main (args :Array[String]) {

    println("7C3 = " + combinations(7, 3))
    println("40C20 = " + combinations(40, 20))
  }

  def combinations(n : Int, k : Int) : Long = {

  var res : Long = 1;

  for (i <- 1 to (k) by 1) {

    res *= (n-i+1)
    res /= (i)
  }

  return res;
  }

  def permutations(n : Int, k : Int) : Long = {

    var res : Long = 1;

    for (i <- n until (n-k) by -1) {

      res *= i;
    }

    return res;
  }
}
