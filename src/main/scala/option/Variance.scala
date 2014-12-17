package option

object Variance {
  def mean(xs: Seq[Double]):Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]):Option[Double] = mean(xs.map(square)).flatMap(x2 => mean(xs).map(x2 - square(_)))
  def square: (Double) => Double = x => x * x

  def map2[A,B,C](a:Option[A], b:Option[B])(f:(A,B) => C):Option[C] = (a,b) match {
    case (Some(av), Some(bv)) => Some(f(av, bv))
    case _ => None
  }

  def sequence[A](as:List[Option[A]]):Option[List[A]] =
    if (as.contains(None)) None
    else Some(as map {case Some(a) => a})
}
