package chapter7 {


  import java.util.concurrent.{ExecutorService, Future}

  import scala.collection.immutable


  object Par {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = ec => ec.submit[A](() => a)

    //todo: try to ensure your implementations respect the contract of the method on that accepts a timeout
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      ec => {
        val fa = a(ec)
        val fb = b(ec)
        ec.submit(() => f(fa.get, fb.get))
      }

    //todo: what impl should be?
    //it should switch computation to different logical thread
    def fork[A](a: => Par[A]): Par[A] = a

    def async[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] = a => unit(f(a))

    def sortPar(l: Par[List[Int]]): Par[List[Int]] = map2(l, unit(()))((a, _) => a.sorted)

    def map[A, B](fa: Par[A])(f: A => B): Par[B] = map2(fa, unit(()))((a, _) => f(a))

    def sortPar_2(l: Par[List[Int]]) = map(l)(_.sorted)

    def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] =
      ec => {
        ec.submit(() => (fa(ec).get, fb(ec).get))
      }


    def map_2[A, B](fa: Par[A])(f: A => B): Par[B] =
      ec => {
        ec.submit(() => f(fa(ec).get))
      }

    def map2_2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = map_2(product(a, b)) { case (a, b) => f(a, b) }

    //such implementation to make execution of each element f(a) from list l in separate execution context
    //    def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    //      def go(list: List[Par[B]]): Par[List[B]] = list match {
    //        case Nil => map(unit(()))(_ => List())
    //        case h :: Nil => map(h)(List(_))
    //        case h :: h1 :: t =>
    //          val current = map2(h, h1) { case (a, b) => List(a, b) }
    //          map2(current, go(t)) { case (l, r) => l ::: r }
    //      }
    //
    //      val list = l.map(a => unit(f(a)))
    //      go(list)
    //    }

    def sequence[A](l: List[Par[A]]): Par[List[A]] = l match {
      case Nil => map(unit(()))(_ => List())
      case h :: t => map2(map(h)(List(_)), sequence(t)) { case (l, r) => l ::: r }
    }

    def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = l.map(asyncF(f))
      sequence(fbs)
    }

    //todo: bad design, in this case it previously calc everything and then filter result
    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork {
      val fbs = l.map(unit(_))
      val p = sequence(fbs)
      map(p)(_.filter(f))
    }

    def parAggregation[A](as: IndexedSeq[A])(z: A)(f: (A, A) => A): Par[A] = parFold(as)(z)(f)(f)

    def parSum(seq: IndexedSeq[Int]): Par[Int] = parAggregation(seq)(0)(_ + _)

    def parFold[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B)(merge: (B, B) => B): Par[B] =
      if (as.size <= 1) unit(as.headOption.map(a => f(a, z)).getOrElse(z))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        val resL: Par[B] = parFold(l)(z)(f)(merge)
        val resR: Par[B] = parFold(r)(z)(f)(merge)
        map2(resL, resR)(merge)
      }

    //list of paragraphs, return count of words across all paragraphs
    //Generalize this function as much as possible
    def wordsCount(str: String): Int = 5 //dummy method
    //    def parCountWords(linesOfText: List[String]): Par[Int] = {
    //      val parsList = linesOfText.map(asyncF(wordsCount))
    //      map(sequence(parsList))(l => l.sum)
    //    }

    def parCountWords(linesOfText: List[String]): Par[Int] =
      parFold(linesOfText.toIndexedSeq)(0) { case (a, b) => b + wordsCount(a) }(_ + _)

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
      map2(
        map2(a, b) { case (a, b) => f.curried(a)(b) }, c) { case (f2, c) => f2(c) }


    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
      map2(
        map3(a,b,c) {case (a,b,c) => f.curried(a)(b)(c)}, d
      ) { case (f2, d) => f2(d) }


    def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
      map2(
        map4(a,b,c,d) {case (a,b,c,d) => f.curried(a)(b)(c)(d)}, e
      ) { case (f2, e) => f2(e) }

    //    def run[A](a: Par[A]): A = ...
  }

}