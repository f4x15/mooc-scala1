package funsets

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   *
   * This is call partial-defined FunSet function
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   *
   * FunSet is `predicate-function`, that determinate element in set or not.
   * For set 1..100 elements we create predicate functions like this:
   *  def setPred(elem: Int) = if (elem >= 1 && elem <= 100) true else false
   *
   * Internals: This set realisation can store single element only
   *  x - test parameter, elem - stored element in set
   *
   * It is partial-defined function. Firstly it store elem only than that
   * call this function we set x-parameter and call the function in `contains`
   */                                 // x - parameter, elem - stored elem in set
  def singletonSet(elem: Int): FunSet = x => x == elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   *
   * All function with result FunSet create new instance of FunSet.
   * As singletonSet / union / intersect / etc
   * It is complex instance which contain s and t function
   */
  def union(s: FunSet, t: FunSet): FunSet = x => { s(x) || t(x) }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = x => { s(x) && t(x) }

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   *
   * if intersect => false, else s(x)
   */
  def diff(s: FunSet, t: FunSet): FunSet = x => {
    !(s(x) && t(x)) && s(x)
  }

  /**
   * Returns the subset of `s` for which `p` holds.
   *
   * Create filter characteristic function: firstly check predicate,
   *  than check set contains
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet =
    x => p(x) && s(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   *
   * For `forall`` need store state when we iterate throw all set elements.
   *  For search all element in set we iterate throw bound+-
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) &&
        !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   *
   * forall and exists are like quantifiers in math sense
   * exists(x),f(x)==true; <=> !(forall(x),f(x)=false)
   * see: https://www.coursera.org/learn/progfun1/discussions/weeks/2/threads/K2f0MLHpEem9FQp29_7a-A/replies/Jx-QBBoTEeeskRI8P5CzrA
   *
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean =
    !(forall(s, x => p(x) == false))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   *
   * If we create FunSet that we create characteristic function on
   *  input with we put all values from -bound..+bound
   * In complex tree-calls we rename variables for avoid escaping a variable
   */
  def map(s: FunSet, f: Int => Int): FunSet = x => {
    exists(s, y => f(y) == x)
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit = {
    println(toString(s))
  }
}

object FunSets extends FunSets
