/**
 *
 * We can missing an implementation
 * And Can't create instance this class by `new``
 **/
abstract class IntSet {
  // add/include element `x` to Set
  def incl(x: Int): IntSet
  // test element in Set?
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def toString = "{" + left + elem + right + "}"

  // create new tree if we return IntSet
  // left, right other are for round. elem for create tree
  def union(other: IntSet): IntSet =
    (left union right ) union other incl elem
}

// There are two type nodes of tree:
class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  // add/include note to tree
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

  override def toString = "."

  def union(other: IntSet): IntSet = other
}

val t = new NonEmpty(3, new Empty, new Empty)
val t1 = t incl 2 incl 5 incl 7
val t2 = t incl 4 incl 7

t1 union t2

val test: Int = 1

// Trait like Java Interfaces with
//  field implementation ability
trait Planar {
  def height: Int
  def width: Int
  def trait List[T]
  class Cons[T](val head: T, val tail: List[T]) extends List[T]
  class Nil[T] extends List[T] = height * width
}

// usening
// class Square extends Shape with Planar with Movable ???