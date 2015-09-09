package fpinscala

object MyModule {
  def abs(n : Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absoulte value of %d id %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // EXERCISE 2.1
  def fib(n: Int): Int = {
    def loop(a: Int, b: Int, i: Int): Int =
      if (i == n) a
      else loop(b, a+b, i+1)

    loop(0, 1, 0)
  }
}
