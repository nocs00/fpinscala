package chapter6

import chapter6.RNG.{Rand, int, ints, positiveMax}
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
        val (ints, rng2) = RNG.ints(100).run(rng)

        ints.size mustBe 100
        ints.toSet.size mustBe 100
      }

      "positiveMax" in {
        val rng = RNG.simple(12345L)
        val rand = RNG.positiveMax(5)
        val (t1, t2) = rand.run(rng)
        println(t1, t2)
      }

      "sequence of transformations" in {
        val rng = RNG.simple(12345L)
        val rand = RNG.sequence(List(RNG.positiveMax(100), RNG.positiveMax(2), RNG.double_2_Rand))
        val result = rand.run(rng)._1

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

        val rand = RNG.map_2(RNG.nextInt_Rand)(_.toString)
        rand.run(rng)._1 mustBe "454757875"

        RNG.map2_2(RNG.nextInt_Rand, RNG.double_2_Rand)((a, b) => s"$a:$b").run(rng)._1 mustBe "454757875:0.4034805881807024"
      }

      "maps-flatMaps with state" in {
        val rng = RNG.simple(12345L)

        val genList_rand: Rand[List[Int]] =
          RNG.positiveMax(20).flatMap(x =>
            RNG.int.flatMap(y =>
              RNG.ints(x).map(xs =>
                xs.map(_ % y))))

        val (list, newRng) = genList_rand.run(rng)
        assert(list.size <= 20)
        assert(list.toSet.size == list.size)
      }

      "maps-flatMaps with for-construct" in {
        val genList =
          for {
            x <- positiveMax(20)
            y <- int
            xs <- ints(x)
          } yield xs.map(_ % y)

        val rng = RNG.simple(12345L)
        val (list, newRng) = genList.run(rng)
        assert(list.size <= 20)
        assert(list.toSet.size == list.size)
      }

      "modify" in {
        val genList =
          for {
            x <- positiveMax(20)
            y <- int
            xs <- ints(x)
          } yield xs.map(_ % y)

        val rng = RNG.simple(12345L)
        val (list, newRng) = genList.run(rng)

        val genList_ModifiedState = genList.modify(_ => seqFromRng(2))
        val (list2, newRng2) = genList_ModifiedState.run(rng)

        //seqFromRng: x = 2, y = 3, ints(2) = 4,5 ; so next is 6
        newRng2.nextInt._1 mustBe 6
      }
    }
  }

  def seqFromRng(returnValue: Int): RNG = new RNG {
    def nextInt = (returnValue, seqFromRng(returnValue+1))
  }

  def constRng(returnValue: Int): RNG = new RNG {
    def nextInt = (returnValue, constRng(returnValue))
  }
}