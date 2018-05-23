package chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)

    def randomPair: ((Int, Int), RNG) = {
      val (i1, rng2) = this.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }


  }

  object RNG {


    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
          ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int],
          simple(seed2))
      }
    }

    def randomPair(rng: RNG): ((Int, Int), RNG) = {
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }

    def positiveInt(rng: RNG): (Int, RNG) = {
      val (i, newRng) = rng.nextInt
      if (i == Int.MinValue) (0, newRng)
      else (Math.abs(i), newRng)
    }

    //returng double 0..1
    def double(rng: RNG): (Double, RNG) = {
      val (i, newRng) = positiveInt(rng)
      val d = i.toDouble / Int.MaxValue
      (d, newRng)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i1, rng2) = rng.nextInt
      val (d1, rng3) = double(rng2)
      ((i1, d1), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), rng2) = intDouble(rng)
      ((d, i), rng2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng1) = double(rng)
      val (d2, rng2) = double(rng1)
      val (d3, rng3) = double(rng2)
      ((d1, d2, d3), rng3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      var list: List[Int] = List()
      if (count <= 0) (list, rng)
      else {
        var newRng = rng
        for (i <- 1 to count) {
          val (int, rng2) = newRng.nextInt
          newRng = rng2
          list = int :: list
        }
        (list, newRng)
      }

    }

    //------------

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def positiveMax(n: Int): Rand[Int] =
      if (n < 0)
        (0, _)
      else
        map(positiveInt)(i => i % n)

    def double_2_Rand: Rand[Double] =
      map(positiveInt)(_.toDouble / Int.MaxValue)

    def double_2(rng: RNG): (Double, RNG) =
      double_2_Rand(rng)

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
      }

    def intDouble_2(rng: RNG): ((Int, Double), RNG) =
      map2(_.nextInt, double)((_, _))(rng)

    def doubleInt_2(rng: RNG): ((Double, Int), RNG) =
      map2(double, _.nextInt)((_, _))(rng)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
      case Nil => throw new IllegalArgumentException("empty transformations list is not allowed")
      case h :: h1 :: Nil => map2(h, h1)((a, b) => List(a, b))
      case h :: Nil => map(h)(a => List(a))
      case h :: h1 :: t =>
        val transformation: Rand[List[A]] = map2(h, h1)((a, b) => List(a, b))
        map2(transformation, sequence(t))((a, b) => a ::: b)
    }

    def ints_2(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(_.nextInt): List[Rand[Int]])(rng)

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng2) = f(rng)
        g(a)(rng2)
      }

    def positiveInt_2(rng: RNG): (Int, RNG) =
      flatMap(int)(
        i =>
          if (i == Int.MinValue) (0, _)
          else (i.abs, _)
      )(rng)

    def map_2[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => (f(a), _))

    def map2_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => {
      flatMap(rb)(b => {
        (f(a, b), _)
      })
    })
  }

  //generalizing RNG as immutable State transitions
  //todo: state probably option?
  case class State[S, +A](run: S => (A, S), var state: S) {

    def run2: A = {
      val (a, s) = run(state)
      state = s
      a
    }

    def flatMap[B](g: A => State[S, B]): State[S, B] = new State[S, B](
      s => {
        val (a, s2) = this.run(s)
        g(a).run(s2)
      }, state)

    def map[B](f: A => B): State[S, B] =
      flatMap(
        a => new State[S, B]((f(a), _), state)
      )

    def get: S = state

    def set(newState: S): Unit = this.state = newState

    //fixme:
    // Error:(177, 12) value flatMap is not a member of type parameter S
    //      s <- get
    // Error:(178, 15) value map is not a member of Unit
    //      _ <- set(f(s))
    def modify(f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

  }

  object State {
    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = sa.flatMap(a => {
      sb.flatMap(b => {
        new State[S, C]((f(a, b), _),
          sa.state) //fixme: what of 2 states sa,sb choose then?
      })
    })

//    def unit[S, A](a: A): State[S, A] = new State[S, A]((a, _))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs match {
      case Nil => throw new IllegalArgumentException("empty transformations list is not allowed")
      case h :: h1 :: Nil => map2(h, h1)((a, b) => List(a, b))
      case h :: Nil => h.map(a => List(a))
      case h :: h1 :: t =>
        val transformation: State[S, List[A]] = map2(h, h1)((a, b) => List(a, b))
        map2(transformation, sequence(t))((a, b) => a ::: b)
    }
  }

  object CandyMachineSimulation {

    sealed trait Input

    case object Coin extends Input

    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int)

    def simulateMachine(inputs: List[Input]): State[Machine, Int] = null

  }

}