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
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prevAcc: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, acc, prevAcc + acc)
    }

    go(n, 1, 0)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def binarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }

    go(0, 0, as.length - 1)
  }

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
      def go(as: Seq[A]): Boolean = {
          as match {
            case Seq(a, b, tail@_*) if gt(a,b) => go(b +: tail)
            case xs: Seq[A] if xs.length < 2 => true
            case _ => false
          }
      }
    go (as)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    val g = (x: A) => (y: B) => f(x,y)
    g(a)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (x: A) => (y: B) => f(x,y)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (x: A, y: B) => f(x)(y)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}

object Runner {

  import Chapter2._

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(s"factorial of 5 is ${factorial(5)}")
    println(s"fib of 6 is ${fib(6)}")

    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibonacci", 6, fib))

    println(binarySearch(Array("Foo", "Bar", "Baz"), "Bar", (a:String, b:String) => a > b  ))

    println(isSorted[String](Array("a", "cbnn", "cc"), (a,b) => a < b))

    val mul10 = partial1[Int, Int, Int](10, (a, b) => a * b)
    println(mul10(5))
    
    val curried = curry[Int, Int, Int]((a,b) => a + b)
    val curried2 = curried(2)
    println(s"${curried2(3)}")

    val g = (a: Int) => String.valueOf(a)
    val f = (b: String) => b.toDouble
    val fg = compose(f, g)
    val fg2 = f compose g
    val fg3 = g andThen f
    println(s"g(5) => ${g(5).getClass} : ${g(5)}")
    println(s"f(g(5)) => ${f(g(5)).getClass} : ${f(g(5))}")
    println(s"compose(f, g) => ${fg(5).getClass} : ${fg(5)}")
    println(s"f compose g => ${fg2(5).getClass} : ${fg2(5)}")
    println(s"g andThen f => ${fg3(5).getClass} : ${fg3(5)}")

  }
}