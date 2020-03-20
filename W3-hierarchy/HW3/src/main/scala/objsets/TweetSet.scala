package objsets

import TweetReader._

import scala.annotation.tailrec
import scala.runtime.Nothing$

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet extends TweetSetInterface {
  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   */
  def filter(p: Tweet => Boolean): TweetSet

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   */
  def mostRetweeted: Tweet

  /**
   * Helper method for `mostRetweeted`
   */
  def mostRetweetedAcc(elem: Tweet): Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   */
  def descendingByRetweet: TweetList

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def filter(p: Tweet => Boolean): TweetSet = this

  // acc is accumulator. It is boundary condition.
  // Simple pass acc next
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Base case
   */
  def union(that: TweetSet): TweetSet = that

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   */
  def mostRetweeted: Tweet = throw new NoSuchElementException("Empty set")

  /**
   * Helper method for `mostRetweeted`
   */
  def mostRetweetedAcc(elem: Tweet): Tweet = elem

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   */
  def descendingByRetweet: TweetList = Nil

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  override def toString: String = "."
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   */
  override def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  /**
   * Return new subset of tree where `p` predicate is true.
   * Iterate over all tree and check `p`. Broadcast result through `recursion`.
   *
   * @param p   - predicate
   * @param acc - accumulate of result
   * @return new `TweetSet` subtree where predicate is true
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    left.filterAcc(p, right.filterAcc( // iterate over all subtrees
      p, if (p(elem)) acc.incl(elem) else acc) // check if need inc curr element,
      // if no need simple pass acc to next
    )
  }

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * It is `the fastest` implementation
   * It is more effective because deep of recursion is less
   * [https://www.coursera.org/learn/progfun1/discussions/weeks/3/threads/KysSP7HpEem9FQp29_7a-A/replies/NLofwbHpEemMUQoC5G__rA[]
   * VS when we go throw left/right we go into recursion deeper and deeper...
   *
   * There are more O-complex solutions:
   * left union right union that incl elem
   * ((left union right) union that) incl elem
   *  etc...
   */
  def union(that: TweetSet): TweetSet =
    (left union (right union (that incl elem)))

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   */
  def mostRetweeted: Tweet = {
    mostRetweetedAcc(this.elem)
  }

  /**
   * Helper method for `mostRetweeted`
   */
  def mostRetweetedAcc(max: Tweet): Tweet = {
    left.mostRetweetedAcc(right.mostRetweetedAcc(
      if (elem.retweets > max.retweets) elem else max))
  }

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   */
  def descendingByRetweet2: TweetList = {
    // what we change what we pass as params
    @tailrec
    def iter(list: TweetList, set: TweetSet): TweetList = {
      // TODO: how test empty set or not???   // how is it more sutable??
      if (set.isInstanceOf[NonEmpty]) { // how do more accurate
        val tweet = set.mostRetweeted
        // list.tail. - how add in reverside order???
        iter(new Cons(tweet, list), set.remove(tweet))
      }
      else list
    }

    iter(Nil, this)
  }

  def descendingByRetweet: TweetList = {
    val most = mostRetweeted
    new Cons(most, remove(most).descendingByRetweet)
  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  override def toString: String = "{" + left + elem + right + "}"
}

trait TweetList {
  def head: Tweet

  def tail: TweetList

  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")

  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(tweet => {
    google.exists(keyword => tweet.text.contains(keyword))
  })

  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(tweet => {
    apple.exists(keyword => tweet.text.contains(keyword))
  })

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
}

object Main extends App {

  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 21))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)

    val set6 = set5.incl(new Tweet("e", "e body", 20))
    val set7 = set6.incl(new Tweet("f", "f body", 20))
    val set8 = set7.incl(new Tweet("g", "g body", 20))
    val set9 = set8.incl(new Tweet("h", "h body", 20))
    val set10 = set9.incl(new Tweet("i", "i body", 20))
    val set11 = set10.incl(new Tweet("j", "j body", 20))
    val set12 = set11.incl(new Tweet("k", "a body", 20))
    val set13 = set12.incl(new Tweet("l", "l body", 20))
    val set14 = set13.incl(new Tweet("n", "n body", 24))
    val set15 = set14.incl(new Tweet("o", "o body", 20))
    val set16 = set15.incl(new Tweet("p", "p body", 20))
    val set17 = set16.incl(new Tweet("r", "r body", 20))
    val set18 = set17.incl(new Tweet("s", "s body", 20))
    val set19 = set18.incl(new Tweet("t", "t body", 20))
    val set20 = set19.incl(new Tweet("u", "u body", 20))
    val set21 = set20.incl(new Tweet("x", "x body", 20))
    val set22 = set21.incl(new Tweet("w", "w body", 20))
    val set23 = set22.incl(new Tweet("z", "z body", 20))
    val set24 = set23.incl(new Tweet("m", "m body", 20))

    val bigSet1 = set5 union set6 union set7 union set8 union set9 union set10 union
      set11 union set12 union set13 union set14 union set15 union set16 union set17 union
      set18 union set19 union set20 union set21 union set22 union set23 union set24

    val bigSet2 = set5 union set6 union set7 union set8 union set9 union set10 union
      set11 union set12 union set13 union set14 union set15 union set16 union set17 union
      set18 union set19 union set20 union set21 union set22 union set23 union set24
  }

  /*  // union logic
  new TestSets {
    val t0 = System.nanoTime()
    val result = bigSet1 union bigSet2
    val t1 = System.nanoTime()
    val elapsed = t1 - t0

    println(s"elapsed: $elapsed ns")
  }
  */

  // mostRetweeted logic

  /*
  new TestSets {
   // println(set2.foreach(x => x.toString))
    val retweet = set24.descendingByRetweet
    println(retweet.foreach( x => println(x)))

  }
   */

  // Print the trending tweets
  GoogleVsApple.trending foreach println
  //println(GoogleVsApple.appleTweets)
}
