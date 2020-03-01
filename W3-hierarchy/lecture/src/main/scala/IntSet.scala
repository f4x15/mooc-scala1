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

  def incl(x: Int): IntSet = {
    var t: IntSet = null
    //                  create copy subtree with current element
    //  left incl x, - create new subtree
    //  create copy of current elements: new NonEmpty(elem, ..., right)
    if (x < elem) t = new NonEmpty(elem, left incl x, right)
    else if (x > elem) t = new NonEmpty(elem, left, right incl x)
    else t = this

    t
  }

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

object Main {

  def main(args: Array[String]): Unit = {
    val t = new NonEmpty(7, new Empty, new Empty)
    val t1 = t incl 5 incl 12 incl 9 incl 13

    val t2 = t1 incl 3
    println(t1)
    println(t2)
  }
}