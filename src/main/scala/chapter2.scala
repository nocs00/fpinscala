package chapter2

object Chapter2 {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prevAcc: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, acc, prevAcc + acc)
    }

    go(n, 1, 0)
  }

}

object Runner {
  import Chapter2._

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(s"factorial of 5 is ${factorial(5)}")
    println(s"fib of 6 is ${fib(6)}")
  }
}