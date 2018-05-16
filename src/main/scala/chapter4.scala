package chapter4 {

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


  object Runner {

    def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = mean(xs)
      .map(m => xs.map(d => math.pow(d - m, 2)))
      .flatMap(seq => mean(seq))

    def main(args: Array[String]): Unit = {

      val opt: Option[Int] = None
      println(opt.map(_.toString))
      val opt1: Option[Int] = Some(123)
      println(opt1.map(_.toString))
      val opt2: Option[Int] = Some(321)

      println(opt1.flatMap(o1 => opt2.map(o2 => o2 - o1)))

      val opt3: Option[Int] = None
      println(opt3.getOrElse(333))


    }
  }

}