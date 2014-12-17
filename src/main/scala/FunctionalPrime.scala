object FunctionalPrime {
  def isPrime(n:Int):Boolean =
    (1 to Math.sqrt(n).toInt count (n % _ == 0)) == 1
}
