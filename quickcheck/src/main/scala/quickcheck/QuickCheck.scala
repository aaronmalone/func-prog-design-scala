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

  /* if you insert into a heap the current minimum of that heap, the new minimum is the same */
  /* bogus 2 */
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /* if you meld a heap with the empty heap, the minimum of the result heap is the min of the non-empty heap */
  /* bogus 5 */
  property("meld empty") = forAll { (h: H) =>
    val melded = meld(h, empty)
    isEmpty(melded) || findMin(melded) == findMin(h)
  }

  /* bogus 1 */
  property("delete only element") = forAll { (n: Int) =>
    val heap = deleteMin(insert(n, empty))
    isEmpty(heap)
  }

/*  property("min of melded heaps should be min of one or other") = forAll { (h1: H, h2: H, n: Int) =>
    val nonEmpty1 = insert(n, h1)
    val nonEmpty2 = insert(n, h2)
    val m1 = findMin(nonEmpty1)
    val m2 = findMin(nonEmpty2)
    val newMin = findMin(meld(nonEmpty1, nonEmpty2))
    newMin == m1 || newMin == m2
  }*/
}
