import scala.annotation.tailrec

// common functions sum of Int between [a,b]
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)

// concreate sum-functions
def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x
def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

// glue before
def sumInts(a: Int, b: Int) = sum(id, a, b)
def sumCubes(a: Int, b: Int) = sum(cube, a, b)
def sumFactorials(a: Int, b: Int) = sum(fact, a, b)

sumInts(1, 10)
sumCubes(1, 10)
sumFactorials(1, 10)

// anonymous functions:
// as def(x: Int) x * x * x
(x: Int) => x * x * x
(x: Int, y: Int) => x + y

def sumInt2(a: Int, b: Int) = sum(x => x, a,b)
def sumCubes2(a: Int, b: Int) = sum(x => x * x * x, a, b)

sumInt2(1, 10)
sumCubes2(1, 10)

def sumTR(f: Int => Int, a: Int, b: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a+1, f(a) + acc)
  }
  loop(a, 0)
}

sumTR(x => x, 1, 5)









