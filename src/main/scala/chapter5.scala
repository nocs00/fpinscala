package chapter5 {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]

    def isEmpty: Boolean = uncons.isEmpty

    def toList: List[A] = uncons.map { case (h, t) => h :: t.toList } getOrElse (Nil)

    def len: Int

    def take(n: Int): Stream[A] =
      if (isEmpty)
        Stream.empty
      else {
        if (n == 0) Stream.empty
        else uncons.map { case (h, t) => Stream.cons(h, t.take(n - 1)) } getOrElse (Stream.empty)
      }

    def takeWhile(p: A => Boolean): Stream[A] =
      if (isEmpty)
        Stream.empty
      else uncons.map { case (h, t) =>
        if (p(h)) Stream.cons(h, t.takeWhile(p))
        else t.takeWhile(p)
      } getOrElse (Stream.empty)

    //lazy second param in function f
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      uncons match {
        case Some((h, t)) =>
          f(h, t.foldRight(z)(f))
        case None =>
          z
      }

    //laziness of second param allow early termination need of full traversal
    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    //todo: why not 'else b' on 44 line to early terminate?
    //looks like side effect
    def takeWhile_2(p: A => Boolean, withDebugInfo: Boolean = false): Stream[A] = foldRight(Stream.empty: Stream[A])((a, b) => {
      //      if (withDebugInfo) println(s"takeWhile_2: a[$a] ; b[${b.toList}] ; p(a)[${p(a)}]")
      if (p(a)) Stream.cons(a, b)
      else Stream.empty
    })

    def map[B](f: A => B): Stream[B] = foldRight(Stream.empty: Stream[B])((a, b) => Stream.cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty: Stream[A])((a, b) => if (f(a)) Stream.cons(a, b) else Stream.empty)

    def append[B >: A](stream: Stream[B]): Stream[B] = foldRight(stream)((a, b) => Stream.cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty: Stream[B])((a, b) => f(a).append(b))

    def map_2[B](f: A => B): Stream[B] = Stream.unfold(uncons)(s => s.map { case (a, sa) => Some((f(a), sa.uncons)) } getOrElse (None))

    def take_2(n: Int): Stream[A] = Stream.unfold((n, uncons)) { case (nn, s) => {
      if (nn == 0) None
      else s.map { case (a, sa) => (a, (nn - 1, sa.uncons)) }
    }
    }

    def zip[B](s: Stream[B]): Stream[(A, B)] = Stream.unfold((uncons, s.uncons)) {
      case (as, bs) =>
        for {
          (a, sa) <- as
          (b, sb) <- bs
        } yield ((a, b), (sa.uncons, sb.uncons))
    }

    def zipAll[AA >: A, B](s: Stream[B], default_A: => AA, default_B: => B): Stream[(AA, B)] = Stream.unfold((uncons, s.uncons))(s => {
      val (opt1, opt2) = s
      val (newA, newSA) = if (opt1.isDefined) opt1.get else (default_A, Stream.empty)
      val (newB, newSB) = if (opt2.isDefined) opt2.get else (default_B, Stream.empty)

      if (s == (None, None)) None
      else Some(((newA, newB), (newSA.uncons, newSB.uncons)))
    })

    //first impl
    def hasSubsequence[B >: A](s: Stream[B], steps: Int = 1, debug: Boolean = false): Boolean = {
      if (debug) println(steps)

      Stream.startsWith(this, s) ||
        uncons.map { case (a, ss) => ss.hasSubsequence(s, steps + 1, debug) }.getOrElse(false)
    }

    // eficient alternative
    //    {
    //      val u1 = uncons
    //      val u2 = s.uncons
    //
    //      if (!u2.isDefined) true
    //      else if (!u1.isDefined && u2.isDefined) false
    //      else {
    //        val (a, sa) = u1.get
    //        val (b, sb) = u2.get
    //        if (a == b) sa.hasSubsequence(sb)
    //        else sa.hasSubsequence(s)
    //      }
    //    }

    //todo: is there easier way to include Stream.empty to the tails except of propagating flag emptyProcessed
    def tails: Stream[Stream[A]] = Stream.unfold((this, false)) {
      case (s, emptyProcessed) =>
        if (s.isEmpty && emptyProcessed) None
        else if (s.isEmpty) Some(Stream.empty, (Stream.empty, true))
        else s.uncons.map { case (a, sa) => (s, (sa, emptyProcessed)) }
    }

    //fixme: get rid of emptyProcessed
    //like foldRight, but returns all intermediate results
    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    //try3: ...
      //todo: in linear time
    //try 2: not good as it takes not linear time using foldRight on each step, so do not reuse intermediate results
      Stream.unfold((this, false)) {
        case (s, emptyProcessed) =>
          if (s.isEmpty && emptyProcessed) None
          else if (s.isEmpty) Some(z, (Stream.empty, true))
          else s.uncons.map { case (a, sa) => (s.foldRight(z)(f), (sa, emptyProcessed)) }
      }

    //try 1: not correct, it groups from beginning: 1; 1,2; 1,2,3; , but need from end: 1,2,3; 2,3; 3
    //      Stream.unfold((z, this)) { case (b, s) => s.uncons.map {
    //        case (a, sa) =>
    //          val newZ = f(a, b)
    //          (newZ, (newZ, sa))
    //      }
    //      }

  }

  object Stream {

    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None

        def len = 0
      }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
        lazy val len = 1 + tl.len
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    def infiniteConstant[A](a: A): Stream[A] = cons(a, infiniteConstant(a))

    def infiniteFrom(n: Int): Stream[Int] = cons(n, infiniteFrom(n + 1))

    //z - initial state
    //f - function to produce next A - value and next S - state
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) map { case (a, s) => cons(a, unfold(s)(f)) } getOrElse (Stream.empty)

    def infiniteFibs: Stream[Int] = apply(0, 1) append unfold((0, 1): Tuple2[Int, Int]) {
      case (p2, p1) =>
        val nextVal = p2 + p1
        val nextState = (p1, nextVal)
        Some((nextVal, nextState))
    }

    def infiniteFrom_2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

    def infiniteConstant_2[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))

    def infiniteOnes_2: Stream[Int] = infiniteConstant_2(1)

    //fixme: is there more efficient way?
    def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = {
      val zipped = s zip s2
      zipped.len == s2.len && zipped.forAll { case (l, r) => l == r }
    }

    //second impl via tails
    def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
      s1.tails exists (startsWith(_, s2))
  }

  case class StreamReader[A](s: Stream[A]) {
    var stream = s

    def next: Option[A] = stream.uncons.map {
      case (value, restOfStream) =>
        stream = restOfStream
        Some(value)
    } getOrElse (None)
  }

  object Runner {
    def main(args: Array[String]): Unit = {
      val zz = Stream(1, 2, 3, 4, 5)
      println(zz.toList)
    }
  }

}