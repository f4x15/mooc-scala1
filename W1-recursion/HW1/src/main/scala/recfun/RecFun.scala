package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def go(acc: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty && acc == 0) true
      else if (chars.isEmpty && acc != 0 || acc < 0) false
      else if (chars.head == '(') go(acc + 1, chars.tail)
      else if (chars.head == ')') go(acc - 1, chars.tail)
      else go(acc, chars.tail) // pass char
    }

    go(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
