package funsets

object Main extends App {
  import FunSets._

  val s1 = singletonSet(1)
  val s123 = union(s1, union(singletonSet(2), singletonSet(3)))
  printSet(s123)
}
