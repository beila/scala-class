object Fibonacci {
  def fib(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fib(n-1) + fib(n-2)
  }

  def fib2(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n == 0) 0
      else if (n == 1) 1
      else go(n-1, acc) //FIXME
    }
    go(n, 0)
  }
}
