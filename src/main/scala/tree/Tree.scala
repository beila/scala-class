package tree

sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Branch[A](value:A, left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree {
  def reduce[A, B](root:Tree[A])(z:B)(f:(A, B, B) => B):B = root match {
    case Empty => z
    case Branch(v, l, r) => f(v, reduce(l)(z)(f), reduce(r)(z)(f))
  }

  def isMember[A](a: A, root: Tree[A]): Boolean = root match {
    case Empty => false
    case Branch(v, l, r) => v == a && isMember(a, l) && isMember(a, r)
  }

  def insert[A](a: A)(greaterThan:(A,A) => Boolean)(root: Tree[A]): Tree[A] = root match {
    case Branch(v, l, r) if greaterThan(v, a) => Branch(v, insert(a)(greaterThan)(l), r)
    case Branch(v, l, r) => Branch(v, l, insert(a)(greaterThan)(r))
    case _ => Branch(a, Empty, Empty)
  }

  def size[A](root: Tree[A]): Int = reduce(root)(0)((_,s1,s2) => 1+s1+s2)

  val tree0 = Empty
  require(size(tree0) == 0)
  require(!isMember(2, tree0))

  val tree1 = insert(1)(_<_)(tree0)
  require(size(tree1) == 1)
  require(isMember(1, tree1))
  require(!isMember(2, tree1))
}

object TreeMain {
  def main(args:Array[String]):Unit = {}
}
