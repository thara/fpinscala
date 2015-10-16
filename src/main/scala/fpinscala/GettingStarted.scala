package fpinscala

object MyModule {
  def abs(n : Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absoulte value of %d is %d."
    msg.format(x, abs(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit =
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 15, factorial))
    (0 to 4).foreach(
      n => println(formatResult("Fibonacci number", n, fib)))
    println(isSorted(Array(1,3,2), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1,2,3), (a: Int, b: Int) => a < b))
    println(isSorted2(Array(1,2,3), (a: Int, b: Int) => a < b))
    println(isSorted2(Array(1,3,2), (a: Int, b: Int) => a < b))

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  // EXERCISE 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, current: Int): Int =
      if (n == 0) current
      else loop(n - 1, current, prev + current)
    loop(n, 0, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  // EXERCISE 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (as.length <= n+1) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false
    loop(0)
  }

  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (as.length <= n) true
      else ordered(as(n-1), as(n)) && loop(n+1)
    loop(1)
  }

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // EXERCISE 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // EXERCISE 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // EXERCISE 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
