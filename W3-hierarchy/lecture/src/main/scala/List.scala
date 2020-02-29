package week3

import scala.annotation.tailrec
import scala.runtime.Nothing$

/**
 * Fundamental structure in most of functional languages is
 * `immutable linked list.` It consist from two building blocks:
 *  - Nil - the empty list
 *  - Cons - a cell containing a element and pointer to the list
 *
 * @tparam T
 **/
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

/**
 * Cons is a cell containing a `element` and `pointer` to the list
 *
 * @tparam T Type of list
 */
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  // others two fields use from `val` definition
}

/**
 * Nil is empty list
 *
 * @tparam T Type of list
 */
class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  // work because `Nothing` of subtype of any type and also subtype `T`
  def head: Nothing = throw new NoSuchFieldException("Nil.head")
  def tail: Nothing = throw new NoSuchFieldException("Nil.tail")
}




object Main {
  // My variant
  def nth[T](n: Int, l: List[T]):T = {
    @tailrec
    def iter(a: Int, l: List[T]): T = {
      if (l.isEmpty) throw new IndexOutOfBoundsException("Index must be less than l.size")
      else if (a == n) l.head
      else iter(a+1, l.tail)
    }

    if (n < 0) throw new IndexOutOfBoundsException("Index must be non negative")
    iter(0, l)
  }

  // Odersky's variant with recursion
  def nth2[T](n: Int, xs: List[T]):T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth2(n-1, xs.tail)
  }

  def main(args: Array[String]): Unit = {
    def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

    println(singleton[Int](1))
    println(singleton[Boolean](true))

    // or we can reduce type-parameters
    singleton(1)
    singleton(true)

    // ----------------------------
    val l = new Cons(4, new Cons(5, new Cons(6, new Nil)))

    assert(nth2(0, l) == 4, "4")
    assert(nth2(1, l) == 5, "5")
    assert(nth2(2, l) == 6, "6")
    //assert(nth(-1, l) == 7, "out bound")

    println (nth2(2, l))
    println (nth2(3, l))
   // println (nth2(-1, l))

  }
}