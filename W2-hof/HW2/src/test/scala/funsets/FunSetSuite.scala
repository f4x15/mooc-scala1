package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * @Ignore annotation.
   *        @Ignore("not ready yet")
   */
   @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       *
       * If singleton set was successful that set contain this element
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contain elements from both sets`: Unit = {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      val s23 = union(s2, s3)

      val s = intersect(s123, s23)
      assert(!contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(contains(s, 3), "Union 3")
    }
  }

  @Test def `diff contain elements from one set w/o intersect`: Unit = {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      val s23 = union(s2, s3)

      val s = diff(s123, s23)
      assert(contains(s, 1), "Union 1")
      assert(!contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `filter hold elements which hold in predicate`: Unit = {
    new TestSets {
      val s123 = union(union(s1, s2), s3)

      val s = filter(s123, x => x >= 2)
      assert(!contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(contains(s, 3), "Union 3")
    }
  }

  @Test def `forall check if predicate hold for all set`: Unit = {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      assert(forall(s123, x => x > 0), ">0")
      assert(!forall(s123, x => x > 2), ">2")
    }
  }

  @Test def `exists check if predicate hold for one and more in set`: Unit = {
    new TestSets {
      val s123 = union(union(s1, s2), s3)

      assert(exists(s123, x => x > 0), "x>0 valid for all")
      assert(exists(s123, x => x > 1), "x>1 valid for {2, 3}")
      assert(!exists(s123, x => x > 4), "x>4 valid for {none}}")
    }
  }

  @Test def `map create new map transformed by f`: Unit = {
    new TestSets {
      val s123 = union(union(s1, s2), s3)

      assert(contains(map(s123, x => x), 3), "x=>x test")
      assert(contains(map(s123, x => x*2), 6), "x=>x*2 test")
    }
  }

  // need comment it for debugging purpose
  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
