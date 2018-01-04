object GettingStarted {
  def factorial(n: Int): Int = {
    println("go!!")
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  // Exercise 2.1
  def fib(n: Int): Int = {
    def loop(n: Int, prev: Int, cur: Int): Int = {
      println(prev)
      if (n == 0) prev
      else  loop(n - 1, cur, prev + cur)
    }
    loop(n, 0, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (gt(as(n), as(n+1))) false
      else go(n+1)
    go(0)
  }

  // Exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
}
