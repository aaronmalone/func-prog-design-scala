package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    number <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
    updatedHeap <- insert(number, heap)
    result <- oneOf(const(empty), const(updatedHeap))
  } yield result

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  val emptyGen = const(empty)

  val nonEmpty: Gen[H] = for {
    n <- arbitrary[Int]
    h <- oneOf(emptyGen, nonEmpty)
  } yield insert(n, h)

  val pairOfNonEmpties = for {
    x <- nonEmpty
    y <- nonEmpty
  } yield (x, y)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert first element") = forAll { (n: Int) =>
    n == findMin(insert(n, empty))
  }

  property("insert two elements") = forAll { (n: Int) => /* seems to get 2 */
    val oneElement = insert(n, empty)
    val twoElements = insert(n + 1, oneElement)
    val overflow = n + 1 < n
    overflow || n == findMin(twoElements)
  }

  property("meld two non empty") = forAll(pairOfNonEmpties) { pair => /* 1 and 5 */
    val h1 = pair._1
    val h2 = pair._2
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = findMin(meld(h1, h2))
    m == m1 || m == m2
  }

  property("delete only element") = forAll { (n: Int) =>
    val h = deleteMin(insert(n, empty))
    isEmpty(h)
  }

  property("min should be existing min or added element") = forAll(nonEmpty) { h =>
    val originalMin = findMin(h)
    val rand = new java.util.Random().nextInt()
    val newMin = findMin(insert(rand, h))
    newMin == rand || newMin == originalMin
  }

  property("meld equivalent heaps") = forAll { (n: Int) =>
    val h1 = insert(n, empty)
    val h2 = insert(n, empty)
    equal(h1, h2) && isEmpty(deleteMin(deleteMin(meld(h1, h2))))
  }

  property("meld equivalent to inserting all elements") = forAll { (h1: H, h2: H) =>
    val meldResult = meld(h1, h2)
    var insertInto = h1
    var deleteFrom = h2
    while(!isEmpty(deleteFrom)) {
      insertInto = insert(findMin(deleteFrom), insertInto)
      deleteFrom = deleteMin(deleteFrom)
    }
    equal(meldResult, insertInto)
  }

  def equal(h1: H, h2: H): Boolean = {
    if (isEmpty(h1)) isEmpty(h2)
    else {
      findMin(h1) == findMin(h2) && equal(deleteMin(h1), deleteMin(h2))
    }
  }

}
