package chapter5

import org.scalatest.{MustMatchers, WordSpec}

class Chapter5Spec extends WordSpec with MustMatchers {

  "Chapter5" should {
    "Stream" should {
      "isEmpty -> true" in {
        val s = Stream()
        s.isEmpty mustBe true
      }

      "isEmpty -> false" in {
        val s = Stream(1, 2, 3)
        s.isEmpty mustBe false
      }

      "uncons check" in {
        val s = Stream(1, 2, 3)
        val u = s.uncons
        u.isDefined mustBe true
        u.get._1 mustBe 1
        u.get._2.isEmpty mustBe false
      }

      "Stream(1,2,3).toList -> List(1,2,3)" in {
        val s = Stream(1, 2, 3)
        s.toList mustBe List(1, 2, 3)
      }

      "Stream(1,2,3,4,5).take(2) -> Stream(1,2)" in {
        val s = Stream(1, 2, 3, 4, 5)
        s.take(2).toList mustBe List(1, 2)
      }

      "Steam(7).take(2) -> Stream(7)" in {
        val s = Stream(7)
        s.take(2).toList mustBe List(7)
      }

      "Stream(1,2,3,4,5).takeWhile(_ < 4) -> Stream(1,2,3)" in {
        val s = Stream(1, 2, 3, 4, 5)
        s.takeWhile(_ < 4).toList mustBe List(1, 2, 3)
      }

      "Stream(1,2,3,4,5,6,7).exists(_ == 5) must take 5 steps due to early termination" in {
        val s = Stream(1, 2, 3, 4, 5, 6, 7)
        //test early termination
        var steps = 0
        s.exists(i => {
          //println(s"$i");
          steps += 1;
          i == 5
        }) mustBe true
        steps mustBe 5
      }

      "Stream(1,2,3,4,5): forAll(_ < 3)=false and takes 3 steps due to early termination; forAll(_ < 6)=true and takes 5 steps" in {
        val s = Stream(1, 2, 3, 4, 5)
        var steps = 0
        s.forAll(i => {
          //println(s"$i");
          steps += 1;
          i < 3
        }) mustBe false
        steps mustBe 3

        steps = 0
        s.forAll(i => {
          //println(s"$i");
          steps += 1;
          i < 6
        }) mustBe true
        steps mustBe 5
      }

      "takeWhile via foldRight must terminate early" in {
        val s = Stream(1, 2, 3, 4, 5)
        var steps = 0
        s.takeWhile_2(i => {
          steps += 1;
          i < 4
        }, true).toList mustBe List(1, 2, 3)
        steps mustBe 4
      }

      "append test" in {
        val s1 = Stream(1, 2, 3, 4, 5)
        val s2 = Stream(6, 7, 8, 9)
        s1.append(s2).toList mustBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
      }


      "map" in {
        val s = Stream(1, 2, 3, 4, 5)
        s.map(_ + 2).toList mustBe List(3, 4, 5, 6, 7)
      }

      "filter" in {
        val s = Stream(1, 2, 3, 4, 5)
        s.filter(_ < 3).toList mustBe List(1, 2)
      }

      "flatMap" in {
        val s1 = Stream(1, 2, 3, 4, 5)
        val s2 = Stream(0)
        s1.flatMap(ss1 => s2 map (ss2 => s"$ss1:$ss2")).toList mustBe List("1:0", "2:0", "3:0", "4:0", "5:0")
      }

      "unfold to produce stream of powers of 2" in {
        val zz = Stream.unfold(0)(s => if (s < 4) Some((Math.pow(2, s).intValue(), s + 1)) else None)
        zz.toList mustBe List(1, 2, 4, 8)
      }

      "test infiniteFibs via unfold" in {
        val fibs = Stream.infiniteFibs
        fibs.takeWhile_2(_ < 100).toList mustBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
      }

      "test StreamReader" in {
        val reader = StreamReader(Stream.infiniteFibs)
        reader.next mustBe Some(0)
        reader.next mustBe Some(1)
        reader.next mustBe Some(1)
        reader.next mustBe Some(2)
        reader.next mustBe Some(3)
        reader.next mustBe Some(5)
      }

      "map_2" in {
        val s = Stream(1, 2, 3, 4, 5)
        val s2 = s.map_2(_.toString + "!")

        s2.toList mustBe List("1!", "2!", "3!", "4!", "5!")
      }


      "take_2" in {
        val s = Stream(1, 2, 3, 4, 5)
        val s2 = s.take_2(3)

        s2.toList mustBe List(1, 2, 3)
      }

      "zip" in {
        val s = Stream(1, 2, 3)
        val s2 = Stream("hello_world".toSeq: _*)
        s.zip(s2).toList mustBe List((1, 'h'), (2, 'e'), (3, 'l'))
      }

      "zipAll" in {
        val s = Stream(1)
        val s2 = Stream("hel".toSeq: _*)
        s.zipAll(s2, -1, '˜').toList mustBe List((1, 'h'), (-1, 'e'), (-1, 'l'))
      }

      "len" in {
        val s = Stream(1, 2, 3)
        s.len mustBe 3
        Stream.cons(5, s).len mustBe 4

        Stream.empty.len mustBe 0
      }

      "hasSubsequence" in {
        val s = Stream(1, 1, 2, 3, 4, 5, 1, 1, 1)
        val s2 = Stream(1, 1, 1)

        s.hasSubsequence(s2) mustBe true

        Stream(1, 2, 3) hasSubsequence Stream(2, 3, 4, 5) mustBe false
      }

      "partial hasSubsequence" in {
        Stream(1, 2, 3).zip(Stream(1, 2)).forAll { case (l, r) => l == r } mustBe true
      }

      "startsWith" in {
        Stream.startsWith(Stream(1, 2, 3, 4), Stream(1, 2)) mustBe true
        Stream.startsWith(Stream(1, 2), Stream(1, 2, 3)) mustBe false
      }

      "tails" in {
        Stream(1, 2, 3).tails.map(_.toList).toList mustBe List(List(1, 2, 3), List(2, 3), List(3), List())
      }

      "scanRight" in {
        Stream(1,2,3).scanRight(0)(_ + _).toList mustBe List(6,5,3,0)
      }
    }
  }
}
