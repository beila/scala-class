package option

trait Either[+E, +A] {
  def map[B](f:A => B): Either[E, B]
  def flatMap[EE >: E, B](f:A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >:E, B, C](b: Either[EE, B])(f:(A,B) => C): Either[EE, C]
}

case class Success[A](a:A) extends Either[Nothing, A] {
  override def map[B](f: (A) => B): Either[Nothing, B] =
    Success(f(a))

  override def map2[EE, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    b.map(f(a, _))
/*    b match {
      case Success(a2) => Success(f(a, a2))
      case Error(e2) => Error(e2)
    }*/

  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] =
    f(a)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this
}

case class Error[E](e:E) extends Either[E, Nothing] {
  override def map[B](f: (Nothing) => B): Either[E, B] =
    this

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] =
    this

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] =
    this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] =
    b
}

object Either {
  Stream()
}
