/**  With Scala 2.13 on Lab machines:

 * In normal circumstances the CLASSPATH is already set for you:

fsc TestTest.scala
scala org.scalatest.run TestTest

 * If you use jar files in your own space:

   With Scala 2.12 and ScalaTest 3.2.2:
fsc -cp ./scalatest-app_2.12-3.2.2.jar TestTest.scala
scala -cp ./scalatest-app_2.12-3.2.2.jar org.scalatest.run TestTest
 * (Once this is working you can set your CLASSPATH in .bashrc) 

*/
import org.scalatest.funsuite.AnyFunSuite
// or import org.scalatest.FunSuite with
// ScalaTest 3.0 or earlier


class TestTest extends AnyFunSuite{ // FunSuite in ScalaTest 3.0
  var set = new IntSet
  test("Empty set has size 0") {
    assert(set.size == 0)
  }
  test("Set adds element") {
    set.add(3)
  }
  test("Set size increments after adding elem") {
    assert(set.size == 1)
  }
  test("Set contains added element") {
    assert(set.contains(3))
  }
  test("Set doesn't contain missing element") {
    assert(!set.contains(4))
  }
  test("Set adds existing element") {
    set.add(3)
  }
  test("Set adds another element") {
    set.add(7)
  }
  test("Set contains another element") {
    assert(set.contains(7))
  }
  test("Set apply operator inits correctly") {
    set = IntSet(2, 4, 6)
    assert(set.contains(4))
    assert(set.contains(2))
    assert(set.contains(6))
    assert(!set.contains(3))
  }
  test("Set apply operator works with duplicate") {
    assert(IntSet(1,2,2,3) == IntSet(1,2,3))
  }
  test("Set equals identical set") {
    assert(set == IntSet(4, 2, 6))
    assert(set.equals(IntSet(4, 2, 6)))
  }
  test("Set doesn't equal different set") {
    assert(set != IntSet(4, 2, 6, 7))
    assert(!set.equals(IntSet(4, 2, 6, 7)))
    assert(set != IntSet(3, 2, 6, 7))
    assert(!set.equals(IntSet(3, 2, 6, 7)))
  }
  test("Set equals itself") {
    assert(set == set)
  }
  test("Non-empty set returns any element") {
    set.any
  }
  test("Empty set throws when calling any") {
    assertThrows[IllegalArgumentException] {
      IntSet().any
    }
  }
  test("Set removes present element") {
    assert(set.remove(2))
  }
  test("Set size decrements when removing present element") {
    assert(set.size == 2)
  }
  test("Set doesn't remove previously present element") {
    assert(set.remove(2) == false)
  }
  test("Set size doesn't decrement after removing previously present element") {
    assert(set.size == 2)
  }
  test("Set doesn't remove missing element") {
    assert(set.remove(3) == false)
  }
  test("Set size doesn't decrement after removing missing element") {
    assert(set.size == 2)
  }
  test("Set is subset of identical set") {
    assert(set.subsetOf(IntSet(1,2,4,6,7,8,9)))
  }
  test("Set is not subset of different set") {
    assert(!set.subsetOf(IntSet(1,3,6,7,8,9)))
  }
  test("Set is not subset of strict subset") {
    assert(!set.subsetOf(IntSet(4)))
  }
  test("Set is subset of itself") {
    assert(set.subsetOf(set))
  }

  // Optional ops
  test("Set union works with no overlap") {
    assert(IntSet(8,3,4).union(IntSet(1,2,5)) == IntSet(1,2,3,4,5,8))
  }

  test("Set union works with total overlap") {
    assert(IntSet(8,3,4).union(IntSet(8,3,4)) == IntSet(3,4,8))
  }

  test("Set union works with total overlap (self)") {
    val s = IntSet(8,3,4)
    assert(s.union(s) == s)
    assert(s.union(s).size == s.size)
  }

  test("Set union works with one empty set") {
    val s = IntSet(8,3,4)
    assert(s.union(IntSet()) == s)
    assert(s == IntSet().union(s))
  }

  test("Set union works with two empty sets") {
    assert(IntSet().union(IntSet()) == IntSet())
  }

  test("Set intersect works with no overlap") {
    assert(IntSet(8,3,4).intersect(IntSet(1,2,5)) == IntSet())
  }

  test("Set intersect works with partial overlap") {
    assert(IntSet(8,3,4).intersect(IntSet(8,3,5)) == IntSet(8,3))
  }

  test("Set intersect works with total overlap") {
    assert(IntSet(8,3,4).intersect(IntSet(8,3,4)) == IntSet(8,3,4))
  }

  test("Set intersect works with one empty set") {
    assert(IntSet(8,3,4).intersect(IntSet()) == IntSet())
  }

  test("Set intersect works with two empty sets") {
    assert(IntSet().intersect(IntSet()) == IntSet())
  }

  test("Set intersect works with one empty set (2)") {
    assert(IntSet().intersect(IntSet(1,2,3)) == IntSet())
  }

  test("Set intersect works with self") {
    var s = IntSet(1,2,3)
    assert(s.intersect(s) == s)
  }

  test("Set map works with empty set") {
    assert(IntSet().map(x => 2*x) == IntSet())
  }

  test("Set map works with non-empty set") {
    assert(IntSet(1,2,3).map(x => 2*x) == IntSet(2,4,6))
  }

  test("Set map works with duplicate results") {
    assert(IntSet(1,2,3,4).map(x => if (x%2==0) 2 else x) == IntSet(1,2,3))
  }

  test("Set filter works with false") {
    assert(IntSet(1,2,3,4).filter(x => false) == IntSet())
  }

  test("Set filter works with true") {
    assert(IntSet(1,2,3,4).filter(x => true) == IntSet(1,2,3,4))
  }

  test("Set filter works with true and false") {
    assert(IntSet(1,2,3,4).filter(x => x%2==0) == IntSet(2,4))
  }
}


/*
//  Corrected:
class TestTest extends AnyFunSuite{
  var x = 0
  test("x=0"){ 
    x=0
    assert(x===0) 
  }
  test("x=1"){ 
    x=1
    assert(x===1)
  }
}
*/  