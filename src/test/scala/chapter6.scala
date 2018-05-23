package chapter6

import org.scalatest.{MustMatchers, WordSpec}

class Chapter6Spec extends WordSpec with MustMatchers {
  "Chapter6" should {
    "Rng" should {
      "randomPair" in {
        val rng = RNG.simple(12345L)
        rng.randomPair._1 mustBe rng.randomPair._1

        val (pair, rng1) = rng.randomPair
        assert(pair != rng1.randomPair._1)
      }

      "positiveInt" in {
        val stepsNumber = 100
        var set: Set[Int] = Set()

        var rng = RNG.simple(12345L)

        for (i <- 1 to 100) {
          val (int, newRng) = RNG.positiveInt(rng)
          set = set + int
          rng = newRng
        }

        set.size mustBe stepsNumber
        set.forall(_ >= 0) mustBe true
      }

      "double" in {
        val stepsNumber = 100

        var rng = RNG.simple(12345L)
        var set: Set[Double] = Set()
        for (i <- 1 to stepsNumber) {
          val (double, newRng) = RNG.double(rng)
          rng = newRng
          set = set + double
        }

        set.size mustBe stepsNumber
      }

      "ints" in {
        val rng = RNG.simple(12345L)
        val (ints, rng2) = RNG.ints(100)(rng)

        ints.size mustBe 100
        ints.toSet.size mustBe 100
      }

      "positiveMax" in {
        val rng = RNG.simple(12345L)
        val rand = RNG.positiveMax(5)
        val (t1, t2) = rand(rng)
        println(t1, t2)
      }

      "sequence of transformations" in {
        val rng = RNG.simple(12345L)
        val rand = RNG.sequence(List(RNG.positiveMax(100), RNG.positiveMax(2), RNG.double_2_Rand))
        val result = rand(rng)._1

        result.size mustBe 3
        assert(result(0).asInstanceOf[Int] >= 0 && result(0).asInstanceOf[Int] <= 100)
        assert(result(1).asInstanceOf[Int] >= 0 && result(1).asInstanceOf[Int] <= 2)
        assert(result(2).asInstanceOf[Double] >= 0.0 && result(2).asInstanceOf[Double] <= 1.0)
      }

      "ints_2" in {
        val rng = RNG.simple(12345L)
        val (ints, rng2) = RNG.ints_2(100)(rng)

        ints.size mustBe 100
        ints.toSet.size mustBe 100
      }

      "positiveInt_2" in {
        val stepsNumber = 100
        var set: Set[Int] = Set()

        var rng = RNG.simple(12345L)

        for (i <- 1 to 100) {
          val (int, newRng) = RNG.positiveInt_2(rng)
          set = set + int
          rng = newRng
        }

        set.size mustBe stepsNumber
        set.forall(_ >= 0) mustBe true
      }

      "flatMap: map_2, map2_2" in {
        val rng = RNG.simple(12345L)

        val rand = RNG.map_2(_.nextInt)(_.toString)
        rand(rng)._1 mustBe "454757875"

        RNG.map2_2(_.nextInt, RNG.double_2_Rand)((a, b) => s"$a:$b")(rng)._1 mustBe "454757875:0.4034805881807024"
      }

//      "state" in {
//        val rngAction = new StateTransition[RNG, Int](_.nextInt, RNG.simple(12345L))
//        val int = rngAction.run2
//
//        rngAction.set(RNG.simple(54321L))
//        val i = rngAction.run2
//        assert(i >= 0)
//      }

      //todo: how to make it work? also books example seems incorrect as methods return tuples not direct values:
      /*

      for {
         x <- int
         y <- int
         xs <- ints(x)
        } yield xs.map(_ % y)

       */

      //      "states with for" in {
      //        val state = new State[RNG, Int](RNG.int)
      //        val rng = RNG.simple(12345L)
      //
      //        for {
      //          (x, r1) <- state.run(rng)
      //          (y, r2) <- state.run(r1)
      //          (xs, r3) <- state.set(RNG.ints(x)).run(r2)
      //        } yield xs.map(_ % y)
      //      }
    }
  }
}