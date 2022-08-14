import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertions.*
import org.scalatest.matchers.should.Matchers._

import scala.util.Random

class Test extends AnyFunSuite:

  test("The array constructor works correctly") {
    IArr() shouldBe IArrLeaf0
    IArr(1) shouldBe IArrLeaf1(1)
    IArr(1,2) shouldBe IArrLeaf2(1,2)
    IArr(1,2,3) shouldBe IArrLeaf3(1,2,3)
    IArr(1,2,3,4) shouldBe IArrLeaf4(1,2,3,4)
    IArr(1,2,3,4,5) shouldBe IArrNode2(IArrLeaf4(1,2,3,4), IArrLeaf1(5))
    IArr(1 to 16 : _*) shouldBe IArrNode4(
      IArrLeaf4(1,2,3,4), IArrLeaf4(5,6,7,8), IArrLeaf4(9,10,11,12), IArrLeaf4(13,14,15,16)
    )
    IArr((1 to 17): _*) shouldBe IArrNode2(
      IArrNode4(IArrLeaf4(1,2,3,4), IArrLeaf4(5,6,7,8), IArrLeaf4(9,10,11,12), IArrLeaf4(13,14,15,16)),
      IArrNode1(IArrLeaf1(17))
    )
  }

  test("isEmpty works correctly") {
    IArr().isEmpty shouldBe true
    IArr(1,2,3,4,5).isEmpty shouldBe false
    IArr().append(IArr()).isEmpty shouldBe true
    IArr(1).append(IArr()).isEmpty shouldBe false
    IArr().append(IArr(100)).isEmpty shouldBe false
  }

  test("append works correctly (simple)") {
    // println(IArr().append(IArr()))
    // println(IArr(1).append(IArr()))
    // println(IArr(1,2,3,4,5).append(IArr(1)).append(IArr(1,2,3,4,5)).append(IArr(1,2,3,4,5)))
    // println(IArr(1,2,3,4,5).append(IArr(1)).append(IArr(1,2,3,4,5)).append(IArr(1,2,3,4,5)).length)
    // IArr(1,2,3,4,5).append(IArr(6)).append(IArr(7,8,9,10,11)).append(IArr(12,13,14,15,16)).iterator.foreach(println)
    // IArr((1 to 17): _*).append(IArr((1 to 170): _*)).append(IArr((500 to 600): _*)).iterator.foreach(println)
    IArr(1,2,3,4,5).append(IArr(6,7,8,9)) shouldBe
      IArrNode3(IArrLeaf4(1,2,3,4),IArrLeaf1(5),IArrLeaf4(6,7,8,9))
    IArr(1,2).append(IArr(3,4)) shouldBe IArrNode2(IArrLeaf2(1,2),IArrLeaf2(3,4))
    IArr().append(IArr(1)) shouldBe IArrLeaf1(1)
    IArr().append(IArr()) shouldBe IArrLeaf0
    IArr(1,2,3,4).append(IArr(5,6,7,8)).append(IArr(9,10,11,12)).append(IArr(13,14,15,16)) shouldBe
      IArr(1 to 16 : _*)

    check {
      val lg = Gen.nonEmptyListOf(Gen.listOfN(10, Gen.choose(1, 10)))
      forAll(lg) { (l: List[List[Int]]) =>
        var arr = IArr(l.head: _*)
        for (i <- 1 until l.length) {
          arr = arr.append(IArr(l(i) : _*))
        }
        arr.iterator.toList == l.flatten
      }
    }(_.withMinSuccessfulTests(200)).status shouldBe Passed
  }

  // Change the condForAppend function in the IArr trait
  // to see the results of different conditions for append.
  test("append works correctly (stress)") {

    check {
      val lg = Gen.nonEmptyListOf(Gen.listOfN(10000, Gen.choose(-100, 100)))
      forAll(lg) { (l: List[List[Int]]) =>
        var arr = IArr(l.head: _*)
        for (i <- 1 until l.length) {
          arr = arr.append(IArr(l(i): _*))
        }
        val dummy = arr(arr.length - 1)
        val dummyToo = arr((arr.length - 1) / 2)
        arr.iterator.toList == l.flatten
      }
    }(_.withMinSuccessfulTests(50)).status shouldBe Passed

  }

  test("length works correctly") {
    IArr().length shouldBe 0
    IArr(1).length shouldBe 1
    IArr(1,2).length shouldBe 2
    IArr(1,2,3).length shouldBe 3
    IArr(1,2,3,4).length shouldBe 4
    IArr(1,2,3,4,5).length shouldBe 5
    IArr(1 to 17 : _*).length shouldBe 17

    check {
      val lg = Gen.nonEmptyListOf(Gen.stringOf(Gen.choose(' ', 'z')))
      forAll(lg) { (l: List[String]) =>
        val rept = Random.nextInt(10)
        val lists = for (_ <- 0 until rept) yield l
        val arr = lists.foldLeft(IArr(""))((acc, ls) => acc.append(IArr(ls: _*)))
        arr.length == l.length * rept + 1
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("height works correctly") {
    IArr().height shouldBe 0
    IArr(1).height shouldBe 0
    IArr(1,2).height shouldBe 0
    IArr(1,2,3).height shouldBe 0
    IArr(1,2,3,4).height shouldBe 0
    IArr(1,2,3,4,5).height shouldBe 1
    IArr(1 to 16 : _*).height shouldBe 1
    IArr(1 to 17 : _*).height shouldBe 2
    IArr(1 to 65 : _*).height shouldBe 3
    IArr(1 to ((1 << 10)+1): _*).height shouldBe 5
    IArr(1 to 16 : _*).append(IArr(1 to 16 : _*))
      .append(IArr(1 to 16 : _*)).append(IArr(1 to 16 : _*)).height shouldBe 2
    IArr(1).append(IArr(1 to 16: _*)).height shouldBe 2
    IArr(1).append(IArr(1 to 17: _*)).height shouldBe 3
    IArr(1).append(IArr(1 to 65: _*)).height shouldBe 4
    IArr(1 to ((1 << 10)+1): _*).append(IArr(1 to ((1 << 10)+1): _*)).height shouldBe 6
  }

  test("array access works correctly") {
    intercept[IndexOutOfBoundsException](IArr()(0))
    IArr(1)(0) shouldBe 1
    IArr(1,2)(1) shouldBe 2
    IArr(1,2)(0) shouldBe 1
    intercept[IndexOutOfBoundsException](IArr(1,2)(3))
    IArr(1,2,3)(2) shouldBe 3
    IArr(1,2,3,4)(3) shouldBe 4
    IArr(1,2,3,4,5)(4) shouldBe 5
    IArr(1 to 17 : _*)(16) shouldBe 17
    IArr(1 to 17 : _*)(10) shouldBe 11
    IArr(1 to 1000 : _*)(998) shouldBe 999
    IArr(1 to 1000 : _*)(500) shouldBe 501
    intercept[IndexOutOfBoundsException](IArrNode1(IArrLeaf4(10,20,30,40))(4))
    IArrNode1(IArrLeaf4(10,20,30,40))(3) shouldBe 40

    intercept[IndexOutOfBoundsException](IArr().append(IArr())(0))
    IArr(1).append(IArr(1))(0) shouldBe 1
    IArr(1,2).append(IArr(1,2))(1) shouldBe 2
    IArr(1,2).append(IArr(1,2))(0) shouldBe 1
    intercept[IndexOutOfBoundsException](IArr(1,2).append(IArr(3))(3))
    IArr(1,2,3).append(IArr(4,5,6))(3) shouldBe 4
    IArr(1,2,3,4).append(IArr(5,6,7,8))(4) shouldBe 5
    IArr(1,2,3,4).append(IArr(5))(4) shouldBe 5
    IArr(1 to 17 : _*).append(IArr(500 to 600 : _*))(20) shouldBe 503
    IArr(1 to 7 : _*).append(IArr(8 to 17 : _*))(10) shouldBe 11
    IArr(1 to 500 : _*).append(IArr(501 to 1000 : _*))(998) shouldBe 999
    IArr(1 to 500 : _*).append(IArr(501 to 1000 : _*))(500) shouldBe 501
    IArr(1,2).append(IArr(3,4))(2) shouldBe 3
  }

  test("update works correctly") {
    IArr(1,2,3,4).update(0, 100) shouldBe IArrLeaf4(100,2,3,4)
    IArr(1,2,3,4,5).update(0,100) shouldBe IArrNode2(IArrLeaf4(100,2,3,4), IArrLeaf1(5))
    IArr(1,2,3,4,5).update(4,100) shouldBe IArrNode2(IArrLeaf4(1,2,3,4), IArrLeaf1(100))
    IArr(1 to 17 : _*).update(0, 100) shouldBe IArrNode2(
      IArrNode4(IArrLeaf4(100,2,3,4), IArrLeaf4(5,6,7,8), IArrLeaf4(9,10,11,12), IArrLeaf4(13,14,15,16)),
      IArrNode1(IArrLeaf1(17))
    )
    check {
      val lg = Gen.nonEmptyListOf(Gen.stringOf(Gen.choose(' ', 'z')))
      forAll(lg) { (l: List[String]) =>
        val idx = Random.nextInt(l.length)
        IArr(l: _*).update(idx, Int.MinValue.toString)(idx) == Int.MinValue.toString
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("slice works correctly") {
    //    val itero = IArrNode2(IArrLeaf4(1,2,3,4),IArrLeaf1(5)).iterator
    //    val iteroSliced = IArrNode2(IArrLeaf4(1,2,3,4),IArrLeaf1(5)).slice(0, 4).iterator
    //    IArrNode2(IArrLeaf4(1,2,3,4),IArrLeaf1(5)).slice(0, 4) shouldBe IArrLeaf4(1,2,3,4)
    check {
      val lg = Gen.nonEmptyListOf(Gen.stringOf(Gen.choose(' ', 'z')))
      forAllNoShrink(lg) { (l: List[String]) =>
        val idx1 = Option(l.length).filter(_ > 1).map(lgth => Random.nextInt(lgth - 1)).getOrElse(0)
        val idx2 = Random.nextInt(l.length - idx1) + idx1
        IArr(l: _*).slice(idx1, idx2) == IArr(l.slice(idx1, idx2): _*)
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("concatenation works correctly") {

    IArr(1,2,3,4).update(0, 100) shouldBe IArrLeaf4(100,2,3,4)
    IArr(1,2,3,4,5).update(0,100) shouldBe IArrNode2(IArrLeaf4(100,2,3,4), IArrLeaf1(5))
    IArr(1,2,3,4,5).update(4,100) shouldBe IArrNode2(IArrLeaf4(1,2,3,4), IArrLeaf1(100))
    IArr(1 to 17 : _*).update(0, 100) shouldBe IArrNode2(
      IArrNode4(IArrLeaf4(100,2,3,4), IArrLeaf4(5,6,7,8), IArrLeaf4(9,10,11,12), IArrLeaf4(13,14,15,16)),
      IArrNode1(IArrLeaf1(17))
    )

    check {
      val lg = Gen.nonEmptyListOf(Gen.stringOf(Gen.choose(' ', 'z')))
      forAll(lg) { (l: List[String]) =>
        val idx = Random.nextInt(l.length)
        IArr(l: _*).update(idx, Int.MinValue.toString)(idx) == Int.MinValue.toString
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed

  }

  test("sample") {
    check {
      forAll { (b: Boolean) => b | !b }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }
