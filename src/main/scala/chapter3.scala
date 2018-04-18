package chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

    def tail[A](xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    def drop[A](xs: List[A], n: Int): List[A] = n match {
      case nn if nn < 2 => tail(xs)
      case _ => drop(tail(xs), n - 1)
    }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
      case Nil => Nil
      case Cons(h, t) => Cons(newHead, t)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  object Runner {

    import chapter3.List._

    def main(args: Array[String]): Unit = {
      val example = Cons(1, Cons(2, Cons(3, Nil)))
      val example2 = List(1, 2, 3)
      val total = sum(example)
      println(example)
      println(example2)
      println(total)

      val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
      println(x)

      val xs = List(1, 2, 3, 4, 5)
      val xs1 = tail(xs)
      val xs2 = drop(xs, 3)
      println(s"$xs, $xs1, $xs2")

      println(s"${dropWhile(xs)(x => x < 3)}")
      println(s"${dropWhile(xs)(x => x > 3)}")

      println(s"${init(xs)}")
    }
  }

}
