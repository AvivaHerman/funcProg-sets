package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 * - run the "test" command in the SBT console
 * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   * - test
   * - ignore
   * - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  //  test("string take") {
  //    val message = "hello, world"
  //    assert(message.take(5) == "hello")
  //  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  //  test("adding ints") {
  //    assert(1 + 2 === 3)
  //  }


  import FunSets._

  trait TestSets {
    val even: Set = x => x % 2 == 0
    val odd: Set = x => math.abs(x) % 2 == 1
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  test("contains is implemented") {

    new TestSets {
      assert(contains(x => true, 100))

      for (i <- -bound to bound by 2) {
        assert(contains(even, i))
        assert(contains(odd, i + 1))
      }

    }
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singelton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }

    new TestSets {
      val s = union(even, odd)

      for (i <- -bound to bound) assert(contains(s, i), "Union even + odd")

    }

    new TestSets {
      val s = union(even, s1)

      for (i <- -bound to bound by 2) assert(contains(s, i), "Union even + s1 contains all evens")

      assert(contains(s, 1), "Union even + s1 contains 1")
      assert(!contains(s, 3), "Union even + s1 doesn't contain 3")
    }
  }

  test("intersect contains all elements") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "intersect 1")
      assert(!contains(s, 2), "intersect 2")
    }

    new TestSets {
      val s = intersect(even, odd)

      for (i <- -bound to bound) assert(!contains(s, i), "intersect even + odd")
    }

    new TestSets {
      val s = intersect(even, s2)

      for (i <- -bound to bound)
        if (i == 2) assert(contains(s, i), "intersect even + s2")
        else assert(!contains(s, i), "intersect even + s2")
    }
  }

  test("diff contains all elements") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "diff 1")
      assert(!contains(s, 2), "diff 2")
    }

    new TestSets {
      val s = diff(even, odd)

      for (i <- -bound to bound by 2) {
        assert(contains(s, i), "diff even + odd")
        assert(!contains(s, i + 1), "diff even + odd")
      }
    }

    new TestSets {
      val s = diff(even, s2)

      for (i <- -bound to bound by 2)
        if (i == 2) assert(!contains(s, i), "diff even + s2")
        else assert(contains(s, i), "diff even + s2")
    }
  }

  test("filter contains all elements") {
    new TestSets {
      val s = filter(even, x => x >= 0)

      for (i <- 0 to bound by 2) {
        assert(contains(s, i), "filter positive from even")
        assert(!contains(s, i + 1), "filter doesn't contain odd")
      }

      for (i <- -bound until 0) {
        assert(!contains(s, i), "filter doesn't contain negatives")
      }
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(even, x => (x % 2) + 4 == 4), "forall even")

      assert(forall(odd, x => x != 2), "forall odd")

      assert(!forall(odd, x => x % 2 == 0))
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(even, x => x == 10), "exists even")

      assert(exists(odd, x => x < -9), "exists odd")

      assert(!exists(odd, x => x % 2 == 0), "exists odd")
    }
  }

  test("map") {
    new TestSets {
      val s = map(odd, x => x - 1)

      for (i <- -bound to bound - 1 by 2) {
        assert(contains(s, i), "map odd to even")
        assert(!contains(s, i + 1))
      }
    }
  }
}
