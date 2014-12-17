package stream

import stream.Stream._

trait Stream[+A] {
  def toList: List[A]

  def ::[B >: A](hd: => B): Stream[B]

  def take(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A]

  def dropWhile(p: A => Boolean): Stream[A]

  def group: Stream[Stream[A]]

  def map[B](f: A => B): Stream[B]

  def length: Int

  def flatMap[B](f: A => Stream[B]): Stream[B]
}

case object Empty extends Stream[Nothing] {
  override def toString: String = "Stream()"

  override def toList: List[Nothing] = List()

  override def ::[A](hd: => A): Stream[A] = {
    lazy val head = hd
    Cons(() => head, () => this)
  }

  override def takeWhile(p: (Nothing) => Boolean): Stream[Nothing] = Empty

  override def dropWhile(p: (Nothing) => Boolean): Stream[Nothing] = Empty

  override def group: Stream[Stream[Nothing]] = Empty

  override def map[B](f: (Nothing) => B): Stream[B] = Empty

  override def length: Int = 0

  override def flatMap[B](f: (Nothing) => Stream[B]): Stream[B] = Empty

  override def take(n: Int): Stream[Nothing] = Empty
}

case class Cons[A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList: List[A] = h() :: t().toList

  override def toString: String = "Stream(" + h() + ", ?)"

  override def ::[B >: A](hd: => B): Stream[B] = {
    lazy val head = hd
    lazy val tail = this
    Cons(() => head, () => tail)
  }

  override def takeWhile(p: (A) => Boolean): Stream[A] = if (p(h())) cons(h(), t().takeWhile(p)) else Empty

  override def dropWhile(p: (A) => Boolean): Stream[A] = if (p(h())) t().dropWhile(p) else this

  override def group: Stream[Stream[A]] = cons(takeWhile(_ == h()), dropWhile(_ == h()).group)

  override def map[B](f: (A) => B): Stream[B] = cons(f(h()), t().map(f))

  override def length: Int = 1 + t().length

  override def flatMap[B](f: (A) => Stream[B]): Stream[B] = ???

  override def take(n: Int): Stream[A] = if (n <= 0) Empty else cons(h(), t().take(n - 1))
}

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) Empty else as.head :: Stream(as.tail: _*)

  def from(n: Int): Stream[Int] = if (n <= 0) Empty else cons(n, from(n + 1))

  def iterate[A](z: A)(f: A => A): Stream[A] = cons(z, iterate(f(z))(f))

  def interleave[A](as: Stream[A], bs: Stream[A]): Stream[A] = (as, bs) match {
    case (Cons(h1, t1), Cons(h2, t2)) => cons(h1(), cons(h2(), interleave(t1(), t2())))
    case _ => Empty
  }
}

object StreamMain {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3))
    println(1 :: 2 :: 3 :: Empty)
    println(Stream(1, 2, 3).toList)

    println(from(3))
    println(from(3).takeWhile(_ < 10).toList)
    println(from(3).dropWhile(_ < 10))

    println(Stream("excesssive".toList: _*).group.toList)
    println(Stream("excesssive".toList: _*).group.toList.map(_.toList.mkString))

    println(iterate(1)(_ * 2).takeWhile(_ < 1024).toList)
    println(iterate(1)(_ * 2).takeWhile(_ < 1024).length)

    println(iterate(1)(_ * 2).take(15).toList)

    println(iterate(1)(_ * 2).takeWhile(_ < 1024).map(_.toString).toList)
    println(iterate(1)(_ * 2).takeWhile(_ < 1024).map(_.toString.getClass.getSimpleName).toList)
  }
}

object Las {
  def apply(n: Int, m: Int): Int = ???

  def lookAndSay(row: Stream[Int]): Stream[Int] = interleave(row.group.map({case Cons(h, _) => h()}), row.group.map(_.length))

  def apply(n: Int): Stream[Stream[Int]] = iterate(Stream(1))(r=>lookAndSay(r))

  def main(args: Array[String]): Unit = {
    println(Las(3).toList.map(_.toList))
    println(Las(6).toList.map(_.toList))
  }
}

