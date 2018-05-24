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

    def positiveInt_Rand: Rand[Int] = new Rand(
      rng => positiveInt(rng)
    )

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

    def ints(count: Int): Rand[List[Int]] = new Rand(rng => {
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
    })

    //------------

    type Rand[+A] = StateTransition[RNG, A]

    val int: Rand[Int] = new Rand(_.nextInt)

    def unit[A](a: A): Rand[A] = new Rand(rng => (a, rng))

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = new Rand(
      rng => {
        val (a, rng2) = s.run(rng)
        (f(a), rng2)
      })

    def positiveMax(n: Int): Rand[Int] =
      if (n < 0)
        new Rand((0, _))
      else
        map(positiveInt_Rand)(i => i % n)

    def double_2_Rand: Rand[Double] =
      map(positiveInt_Rand)(_.toDouble / Int.MaxValue)

    def double_2(rng: RNG): (Double, RNG) =
      double_2_Rand.run(rng)

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = new Rand(
      rng => {
        val (a, rng2) = ra.run(rng)
        val (b, rng3) = rb.run(rng2)
        (f(a, b), rng3)
      })

    def intDouble_2(rng: RNG): ((Int, Double), RNG) =
      map2(nextInt_Rand, double_2_Rand)((_, _)).run(rng)

    def nextInt_Rand: Rand[Int] =
      new Rand(_.nextInt)

    def doubleInt_2(rng: RNG): ((Double, Int), RNG) =
      map2(double_2_Rand, nextInt_Rand)((_, _)).run(rng)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
      case Nil => throw new IllegalArgumentException("empty transformations list is not allowed")
      case h :: h1 :: Nil => map2(h, h1)((a, b) => List(a, b))
      case h :: Nil => map(h)(a => List(a))
      case h :: h1 :: t =>
        val transformation: Rand[List[A]] = map2(h, h1)((a, b) => List(a, b))
        map2(transformation, sequence(t))((a, b) => a ::: b)
    }

    def ints_2(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(nextInt_Rand): List[Rand[Int]]).run(rng)

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = new Rand(
      rng => {
        val (a, rng2) = f.run(rng)
        g(a).run(rng2)
      })

    def positiveInt_2(rng: RNG): (Int, RNG) =
      flatMap(int)(
        i =>
          if (i == Int.MinValue) new Rand((0, _))
          else new Rand((i.abs, _))
      ).run(rng)

    def map_2[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => new Rand((f(a), _)))

    def map2_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => {
      flatMap(rb)(b => {
        new Rand((f(a, b), _))
      })
    })
  }

  //generalizing RNG as immutable State transitions
  case class StateTransition[S, +A](run: S => (A, S)) {

    def flatMap[B](g: A => StateTransition[S, B]): StateTransition[S, B] = new StateTransition[S, B](
      s => {
        val (a, s2) = this.run(s)
        g(a).run(s2)
      })

    def map[B](f: A => B): StateTransition[S, B] =
      flatMap(a => new StateTransition[S, B]((f(a), _)))

    //todo: dont understand how to implement this so modify is valid
    //todo: for example what have to be signature of 'get' to return state is for-construct
    //    def get: Option[S] = None
    //
    //    def set(newState: S): StateTransition[S, A] = new StateTransition[S, A](_ => this.run(newState))
    //
    //    def modify(f: S => S): StateTransition[S, Unit] = for {
    //      s <- get
    //      _ <- set(f(s))
    //    } yield ()

    //just workaround
    def modify(f: S => S): StateTransition[S, Unit] = new StateTransition(s => {
      val (a, ss) = run(f(s))
      ((), ss)
    })
  }

  object StateTransition {
    def map2[S, A, B, C](sa: StateTransition[S, A], sb: StateTransition[S, B])(f: (A, B) => C): StateTransition[S, C] = sa.flatMap(a => {
      sb.flatMap(b => {
        new StateTransition[S, C]((f(a, b), _))
      })
    })

    def unit[S, A](a: A): StateTransition[S, A] = new StateTransition[S, A]((a, _))

    def sequence[S, A](fs: List[StateTransition[S, A]]): StateTransition[S, List[A]] = fs match {
      case Nil => throw new IllegalArgumentException("empty transformations list is not allowed")
      case h :: h1 :: Nil => map2(h, h1)((a, b) => List(a, b))
      case h :: Nil => h.map(a => List(a))
      case h :: h1 :: t =>
        val transformation: StateTransition[S, List[A]] = map2(h, h1)((a, b) => List(a, b))
        map2(transformation, sequence(t))((a, b) => a ::: b)
    }
  }


  //-------------candies here

  object CandyMachineSimulation {

    type MachineAction = StateTransition[Machine, Int]

    val noAction: MachineAction = new MachineAction(m => (m.coins, m))

    sealed trait Input

    case object Coin extends Input

    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int) {
      def ignoreInput: (Int, Machine) = (coins, this)
    }

    def simulateMachine(inputs: List[Input]): MachineAction = inputs match {
      case Nil => noAction
      case h :: Nil => processInput(h)
      case h :: t => StateTransition.map2(processInput(h), simulateMachine(t))((_,b) => b)
    }

    def processInput(input: Input): MachineAction = new MachineAction(machine => {
      if (machine.candies <= 0) machine.ignoreInput
      else input match {
        case Coin =>
          if (machine.locked)
            (machine.coins + 1, machine.copy(locked = false, coins = machine.coins + 1))
          else
            machine.ignoreInput

        case Turn =>
          if (machine.locked)
            machine.ignoreInput
          else
            (machine.coins, machine.copy(locked = true, machine.candies - 1))
      }
    })
  }

}
