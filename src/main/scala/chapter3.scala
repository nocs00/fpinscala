package chapter3 {

  import scala.annotation.tailrec

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

    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(l: List[Int]) =
      foldRight(l, 0.0)(_ + _)

    def product2(l: List[Double]) =
      foldRight(l, 1.0)(_ * _)

    def length[A](l: List[A]): Int =
      foldRight(l, 0)((_, b) => 1 + b)

    @tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def sum3(l: List[Int]) = foldLeft(l, 0.0)(_ + _)

    def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

    def append[A](a1: List[A], a2: A): List[A] = a1 match {
      case Nil => Cons(a2, Nil)
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    def reverse[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => append(reverse(xs), x)
    }

    def reverseWithFold[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

    //    def append2[A](a1: List[A], a2: List[A]): List[A] = {
    //      foldLeft(a1, a2)((b,a) => a match {
    //          case Cons(h, Nil) =>
    //          case _ => a
    //        })
    //
    //    }

    def flatten[A](l: List[List[A]]): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => append(h, flatten(t))
    }

    def map[A, B](l: List[A])(f: A => B): List[B] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

    def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
      case Cons(h, t) => filter(t)(f)
    }

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Nil => Nil
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }

    def headOption[A](l: List[A]): Option[A] = l match {
      case Nil => None
      case Cons(h, t) => Some(h)
    }

    def merge[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = l1 match {
      case Nil => l2
      case Cons(h, t) => Cons(headOption(l2).map(f(_, h)).getOrElse(h), merge(t, tail(l2))(f))
    }

    def merge2[A](list1: List[A], list2: List[A])(mergeFunction: (A, A) => A): List[A] = {

      def go(list1: List[A], list2: List[A]): List[A] = {
        val value1 = headOption(list1)
        val value2 = headOption(list2)

        val mergedOption = value1.flatMap(v1 => value2.map(v2 => mergeFunction(v1, v2)))
        mergedOption match {
          case None => go(tail(list1), tail(list2))
          case Some(mergedValue) => Cons(mergedValue, go(tail(list1), tail(list2)))
        }
      }

      go(list1, list2)
    }

    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
      def go(l: List[A], sub2: List[A]): Boolean = l match {
        case Nil => sub2 == Nil
        case Cons(h, t) => sub2 match {
          case Nil => true
          case Cons(hh, tt) =>
            if (h == hh) go(t, tt)
            else go(t, sub)
        }
      }

      go(l, sub)
    }

  }

  sealed trait Tree[+A]

  case object EmptyNode extends Tree[Nothing]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](tree: Tree[A]): Int = {
      def go(tree: Tree[A], acc: Int): Int = {
        tree match {
          case EmptyNode => acc
          case Leaf(value) => acc + 1
          case Branch(l, r) => go(l, 0) + go(r, 0) + acc
        }
      }

      go(tree, 0)
    }

    def maximum(t: Tree[Int]): Option[Int] = t match {
      case EmptyNode => None
      case Leaf(value) => Some(value)
      case Branch(l, r) =>
        val lv = maximum(l)
        val rv = maximum(r)
        if (lv.isEmpty) rv
        else if (rv.isEmpty) lv
        else lv.flatMap(lvm => rv.map(rvm => lvm max rvm))
    }

    def depth[A](t: Tree[A]): Int = {
      def go(t: Tree[A], acc: Int): Int = t match {
        case EmptyNode => acc
        case Leaf(_) => acc
        case Branch(l, r) => acc + 1 + (go(l, 0) max go(r, 0))
      }

      go(t, 0)
    }

    def map[A](t: Tree[A])(f: A => A): Tree[A] = t match {
      case EmptyNode => EmptyNode
      case Leaf(value) => Leaf(f(value))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    //todo: think how to write fold for this tree so it is possible to implement size, max, depth and map via it
    def fold[A, B](t: Tree[A], z: B)(f: (Option[A], B) => B): B = t match {
      case EmptyNode => z
      case Leaf(value) => f(Some(value), z)
      case Branch(l, r) =>
        val res1 = f(None, z)
        val res2 = fold(l, res1)(f)
        val res3 = fold(r, res2)(f)
        res3
    }
//
//    def size2[A](tree: Tree[A]): Int = fold(tree, 0)((tp, v, acc) => tp match {
//      case Leaf.getClass => acc+1
//      case _ => acc
//    })
//
//    //todo: fix branch case
//    def maximum2(t: Tree[Int]): Option[Int] = fold(t, None:Option[Int])((tp, v, acc) => tp match {
//      case EmptyNode.getClass => acc
//      case Leaf.getClass => v.map(vm => acc.map(am => vm max am).getOrElse(vm))
//      case Branch.getClass => acc
//    })

    //todo: is it real to implement via fold?
//    def depth2[A](t: Tree[A]): Int = fold(t, 0)((v,acc) => acc + 1)

//    def map2[A](t: Tree[A])(f: A => A): Tree[A] = fold(t, null: Tree[A])((v, acc) =>  )
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

      val ex8 = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
      println(ex8)
      println(length(List("a", 33.0, 3, 4, 5, 6, 7)))
      println(foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _))
      println(append(List(1, 2, 3), 4))
      println(reverse(List(1, 2, 3)))
      println(reverseWithFold(List(1, 2, 3)))

      println(flatten(List(List(1, 2, 3), List(5, 9, 0), List(-5, -9, 0))))
      println(map(List(1, 2, 3))((x: Int) => x + 1))
      println(map(List(1.1, 2.2, 3.3))((x: Double) => x.toString))
      println(filter(List(1, 2, 3, 4, 5, 6))((x: Int) => x % 2 == 0))
      println(flatMap(List(1, 2, 3))((x: Int) => List(x, x + 1, x + 2)))
      println(merge(List(1, 2, 3), List(4, 5, 6))(_ + _))

      println(hasSubsequence(List(1, 2, 3, 4, 5), List(1))) //true
      println(hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2))) //true
      println(hasSubsequence(List(1, 2, 3, 4, 5), List(4, 5))) //true
      println(hasSubsequence(List(1, 2, 3, 4, 5), List(5))) //true
      println(hasSubsequence(List(1, 2, 3, 4, 5), List(5, 6))) //false
      println(hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3))) //true
      println(hasSubsequence(List(1, 2, 3, 4, 5), List(2, 4))) //false
      println(hasSubsequence(List(1, 2, 3, 4, 5), Nil)) //true
      println(hasSubsequence(Nil, List(2, 3))) //false

      val tree1: Tree[Int] =
        Branch(
          Branch(
            Leaf(3),
            Branch(
              Leaf(2),
              Leaf(18)
            )
          ),
          Branch(
            Leaf(1),
            Leaf(7)
          )
        )
      println(Tree.size(tree1))
      println(Tree.maximum(tree1))
      println(Tree.depth(tree1))
      println(Tree.map(tree1)(value => 0))

//      println(Tree.fold(tree1, 0)((el, acc) => acc + el))
//      println(Tree.size2(tree1))
//      println(Tree.maximum2(tree1))
//      println(Tree.depth2(tree1))
//      println(Tree.map2(tree1)(value => 0))
    }
  }

}

