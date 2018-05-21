package chapter4 {

  import java.util.regex.{Pattern, PatternSyntaxException}

  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

    //todo: It is fine to use pattern matching, though you should be able to implement all the functions besides and without resorting to pattern matching
    def flatMap[B](f: A => Option[B]): Option[B] = map(f) match {
      case None => None
      case Some(optB) => optB
    }

    def get[B >: A]: B = this match {
      case Some(v) => v
      case None => throw new IllegalStateException("empty option")
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case a@Some(_) => a
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(v) if f(v) => Some(v)
      case _ => None
    }

    def isEmpty: Boolean = this == None

    def isDefined: Boolean = !isEmpty
  }

  case object None extends Option[Nothing]

  case class Some[+A](value: A) extends Option[A]


  case object FunctionsWithOptions {
    def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = mean(xs)
      .map(m => xs.map(d => math.pow(d - m, 2)))
      .flatMap(seq => mean(seq))

    def pattern(s: String): Option[Pattern] =
      try {
        Some(Pattern.compile(s))
      } catch {
        case e: PatternSyntaxException => None
      }

    def mkMatcher(pat: String): Option[String => Boolean] =
      pattern(pat) map (p => (s: String) => p.matcher(s).matches)

    def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
      for {
        f <- mkMatcher(pat)
        g <- mkMatcher(pat2)
      } yield f(s) && g(s)

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
      aa <- a map f.curried
      bb <- b map aa
    } yield bb

    def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = map2(mkMatcher(pat1), mkMatcher(pat2))((f, g) => f(s) && g(s))

    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => None
      case h :: Nil => h.map(List(_))
      case h :: t => map2(h, sequence(t))((hh, tt) => hh :: tt)
    }

    def parsePatterns(a: List[String]): Option[List[Pattern]] = sequence(a map pattern)

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => None
      case h :: Nil => f(h).map(List(_))
      case h :: t => map2(f(h), traverse(t)(f))((hh, tt) => hh :: tt)
    }
  }

  case object FunctionWithEither {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)


    def safeDiv(x: Double, y: Double): Either[Exception, Double] =
      try {
        Right(x / y)
      } catch {
        case e: Exception => Left(e)
      }

    def sequence[A](a: List[Either[String, A]]): Either[String, List[A]] = a match {
      case Nil => Left("empty list")
      case h :: Nil => h.map(List(_))
      case h :: t => h.map2(sequence(t))((hh, tt) => hh :: tt)
    }

    def traverse[A, B](a: List[A])(f: A => Either[String, B]): Either[String, List[B]] = a match {
      case Nil => Left("empty list")
      case h :: Nil => f(h).map(List(_))
      case h :: t => f(h).map2(traverse(t)(f))((hh, tt) => hh :: tt)
    }

    object PersonTest {

      case class Person(name: Name, age: Age)

      sealed class Name(val value: String)

      sealed class Age(val value: Int)

      def mkName(name: String): Either[String, Name] =
        if (name == "" || name == null) Left("Name is empty.")
        else Right(new Name(name))

      def mkAge(age: Int): Either[String, Age] =
        if (age < 0) Left("Age is out of range.")
        else Right(new Age(age))

      def mkPerson(name: String, age: Int): Either[List[String], Person] =
        mkName(name).map2_2(mkAge(age))(Person(_, _))
    }
  }

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) =>
        Left(e)
      case Right(a) =>
        Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) =>
        Left(e)
      case Right(a) =>
        f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => b
      case Right(a) => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
      case Left(e) => Left(e)
      case Right(a) =>
        b match {
          case Left(ee) => Left(ee)
          case Right(bb) => Right(f(a, bb))
        }
    }

    def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] = {
      var errors: List[EE] = Nil

      this match {
        case Left(e) =>
          errors = e :: errors
          b match { case Left(e) => errors = e :: errors }
          Left(errors)
        case Right(a) =>
          b match {
            case Left(ee) =>
              errors = ee :: errors
              Left(errors)
            case Right(bb) => Right(f(a, bb))
          }
      }
    }
  }

  case class Left[+E, +A](value: E) extends Either[E, A]

  case class Right[+E, +A](value: A) extends Either[E, A]


  object Runner {

    import chapter4.FunctionWithEither._

    def main(args: Array[String]): Unit = {

      //      val opt: Option[Int] = None
      //      println(opt.map(_.toString))
      //      val opt1: Option[Int] = Some(123)
      //      println(opt1.map(_.toString))
      //      val opt2: Option[Int] = Some(321)
      //
      //      println(opt1.flatMap(o1 => opt2.map(o2 => o2 - o1)))
      //
      //      val opt3: Option[Int] = None
      //      println(opt3.getOrElse(333))
      //
      //
      //      println(map2(None: Option[Int], Some(3))(_ + _))
      //      println(map2(Some(3), None: Option[Int])(_ + _))
      //      println(map2(None: Option[Int], None: Option[Int])(_ + _))
      //      println(map2(Some(3), Some(1))(_ + _))
      //
      //      println(sequence(List(Some(1), None, Some(3))))
      //      println(sequence(List(Some(1), Some(2), Some(3))))
      //      println(sequence(List(Some(1), Some(2), None)))
      //      println(sequence(List(None, Some(2), Some(3))))

      //with right
      for {
        age <- Right(42)
        name <- Right("Petro")
        salary <- Right(1000000.0)
      } yield println(s"employee($name, $age, $salary)")

      //with left fails to get into yield section
      for {
        age <- Right(42)
        name <- Left("invalid name")
        salary <- Right(1000000.0)
      } yield println(s"$age : $name : $salary")

      //the same as prev for comprehension
      val forResult = Right(42).flatMap(rr => Left("invalid name").flatMap(ll => Right(1000.0).map(rrr => s"$rr : $ll : $rrr")))

      println(sequence(List(Right(1), Left("bb"), Left("aa"), Right(3))))
      println(sequence(List(Right(1), Right(2), Right(3))))
      println(sequence(List(Right(1), Right(2), Left("bb"))))
      println(sequence(List(Left("bb"), Right(2), Right(3))))
      println(sequence(List()))


      val personTest = PersonTest.mkPerson("", -1)
      println(personTest)

    }
  }

}