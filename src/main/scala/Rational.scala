class Rational(numerator:Int, denominator:Int) {
  require(denominator != 0, "Division By Zero")

  private def gcd( a: Int, b: Int):Int = if (b == 0) a else gcd(b, a%b)
  private val g = gcd(numerator, denominator)
  val n = numerator / g
  val d = denominator / g

  def this(numerator:Int) = this(numerator, 1)

  override def toString = if (d == 1) n.toString else n + "/" + d

  def + (that:Rational):Rational =
    new Rational(this.n*that.d + that.n*this.d, this.d*that.d)
}

object Rational {
  implicit def fromInt(n:Int):Rational = new Rational(n)

  def main(args:Array[String]):Unit = {
    println(new Rational(1,2) + 2)
  }
}
