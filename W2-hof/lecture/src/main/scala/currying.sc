// sum *function* that return *other function*
def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)

  sumF
}

def fact(x: Int): Int = {
  if (x == 0) 1
  else x * fact(x - 1)
}

// ordinary functions
def sumInts = sum(x => x)
def sumCubes = sum(x => x * x * x)
def sumFactorials = sum(fact)

sumInts(1, 10) + sumCubes(1, 3)

//---
def cube(x: Int): Int = x * x * x

// we can avoid middle-sumCubes function like this:
sum(cube)(1, 10)
// it is equalent of:
sum(cube)(1, 10) == (sum(cube)) (1, 10)
// firstly we calculate function (sum(cube))
//  and then we evaluate *on result of
//  previous functions* sumOfCube(1, 10)

// other shortest description of sum
def sum3(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)

// product of function on interval
def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

product(x => x * x)(3, 4)

// factorial in term of product function:
def fact2(n: Int) = product(x => x)(1, n)

fact2(5)

// write generealise function that combine sum and product
//  product functions: base/unit value and multiple/action
//  mapreduce function that map function on the interval
//  and combined it:
def mapReduce(f: Int => Int, combine: (Int, Int) => Int,
              zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

def product2(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a,b)

def sum2(f: Int => Int)(a: Int, b:Int) =
  mapReduce(f, (x,y)=>x+y, 0)(a, b)

product2(x => x)(1, 3)
sum2(x => x)(1, 5)