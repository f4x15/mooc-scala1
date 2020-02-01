

def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

gcd(14, 21)

def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n-1)

factorial(5)
// tail recursion version of factorial
//  the last operator is self-function call
import scala.annotation.tailrec

def factorialTR(n: Int): Int = {
  @tailrec
  def go (res: Int, n: Int): Int =
    if (n == 0) res
    else go(res*n, n-1)

  go (1, n)
}

factorialTR(5)