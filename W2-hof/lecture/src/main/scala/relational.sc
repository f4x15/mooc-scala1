// It is primary constructor
class Rational(x: Int, y: Int) {
  require(y > 0, "demoninator must be positive")

  // It is secondary, aditions constructor
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  val numer = x
  val denom = y

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  def mul(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)

  def div(that: Rational) =
    new Rational(
      numer * that.denom,
      denom * that.numer
    )

  def < (that: Rational) =
    this.numer * that.denom < this.denom * that.numer

  def max(that: Rational) =
    if (this < that) that else this

  override def toString = {
    val g = gcd(x, y)
    numer / g + "/" + denom / g
  }
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
//x.sub(y).sub(z)
y + y
x < y // infix notation
x max y
x - y

//val strange = new Rational(1, 0)

new Rational(2)
val test = new Rational(70, 49)
println(test.numer + " " + test.denom)