package list

sealed trait List[+B]
case class Cons[A](head:A, tail:List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  def apply[A](as:A*):List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))
  }

  def init[A](as:List[A]):List[A] = as match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
  require(init(List(1,2,3)) == List(1,2))

//  val last: List[Int] => Int = {
  def last[A](as:List[A]):A = as match {
    case Nil => throw new NoSuchElementException
    case Cons(h, Nil) => h
    case Cons(h, t) => last(t)
  }
  require(last(List(1,2,3)) == 3)

  def append[A](as:List[A], bs:List[A]):List[A] = as match {
    case Nil => bs
    case Cons(h, t) => Cons(h, append(t, bs))
  }
  require(append(List(1,2), List(3,4)) == List(1,2,3,4))

  def reverse[A](as:List[A]):List[A] = as match {
    case Nil => Nil
    case _ => Cons(last(as), reverse(init(as)))
  }
  require(reverse(List(1,2,3)) == List(3,2,1))

  def take[A](n:Int, as:List[A]):List[A] = (n, as) match {
    case (_, Cons(h, t)) if n > 0 => Cons(h, take(n-1, t))
    case _ => Nil
  }
  require(take(2, List(1,2,3)) == List(1,2))

  def drop[A](n:Int, as:List[A]):List[A] = (n, as) match {
    case (_, Cons(_, t)) if n > 0 => drop(n-1, t)
    case _ => as
  }
  require(drop(2, List(1,2,3,4)) == List(3,4), drop(2, List(1,2,3,4)).toString)

  def takeWhile[A](as:List[A])(predicate:A=>Boolean):List[A] = as match {
    case Cons(h, t) if predicate(h) => Cons(h, takeWhile(t)(predicate))
    case _ => Nil
  }
  require(takeWhile(List(1,2,3,4,1,2))(_ < 3) == List(1,2))

  def dropWhile[A](as:List[A])(predicate:A=>Boolean):List[A] = as match {
    case Cons(h, t) if predicate(h) => dropWhile(t)(predicate)
    case _ => as
  }
  require(dropWhile(List(1,2,3,4,1,2))(_ < 3) == List(3,4,1,2))

/*  def map[A, B](as:List[A]) (f:A=>B):List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }*/
  require(map(List(1,2,3))(_.toString) == List("1","2","3"))

  def zip[A, B](as: List[A], bs: List[B]):List[(A,B)] = (as, bs) match {
    case (Cons(ah,at), Cons(bh, bt)) => Cons((ah, bh), zip(at, bt))
    case _ => Nil
  }
  require(zip(List(1,2,3), List(true,false)) == List((1,true),(2,false)))

  def fill[A](n:Int, a:A):List[A] = {
    if (n <= 0) Nil
    else Cons(a, fill(n-1, a))
  }
  require(fill(3,'a') == List('a','a','a'))

  def flatMap[A, B](as: List[A])(f: A => List[B]):List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }
  require(flatMap(List(1,2,3))(n => fill(n,n)) == List(1,2,2,3,3,3))

  def product[A, B](as: List[A], bs: List[B]): List[(A,B)] =
    flatMap(as)(a => map(bs)(b => (a,b)))
  println(product(List(1,2,3),List('a','b','c')))

  def fold[A,B](as:List[A])(z:B)(f:(A,B)=>B):B = as match {
    case Nil => z
    case Cons(h, t) => f(h, fold(t)(z)(f))
  }

  def map[A, B](as:List[A])(f:A=>B):List[B] = fold(as)(Nil:List[B])((h, t) => Cons(f(h), t))
}

object ListMain {
  def main(args:Array[String]):Unit = {}
  val oneTwoThree = Cons(1, Cons(2, Cons(3, Nil)))
  println(oneTwoThree)
  println(List(1,2,3))
}
