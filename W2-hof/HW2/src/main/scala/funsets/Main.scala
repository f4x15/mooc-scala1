package funsets

object Main extends App {
  import FunSets._

  val s1 = singletonSet(1)
  val s123 = union(s1, union(singletonSet(2), singletonSet(3)))
 // val g123_same = map(s123, x => x)
  val g123_time2 = map(s123, x => x * 200)
 // printSet(g123_same)
  printSet(g123_time2)
}
