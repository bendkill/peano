object Desugarer {
  def toNat (n: Int) : ast.Term = {
    if (n == 0)
      ast.Zero
    else
      ast.Succ(toNat(n-1))
  }

  def nat (s: String) : ast.Term = {
    toNat(s.toInt)
  }

  def bool (b:Boolean) : ast.Term = if (b) ast.True else ast.False

  def mag(d: String) : ast.Term = toNat(Math.pow(10, d.length()).toInt)

  def apply (st: sast.Term) : ast.Term = st match {
    case sast.Empty => ast.Empty
    case sast.True => ast.True
    case sast.False => ast.False
    case sast.Natural(n) => nat(n)
    case sast.Decimal(isNeg, base, dec) =>
      ast.Add(
        ast.Rational(ast.Integer(bool(isNeg), nat(base)), ast.Succ(ast.Zero)),
        ast.Rational(ast.Integer(ast.False, nat(dec)), mag(dec))
      )
    case sast.Negate(t1) => ast.Negate(this(t1))
    case sast.Positive(t1) => ast.Positive(this(t1))
    case sast.Greater(t1, t2) => ast.Greater(this(t1), this(t2))
    case sast.Even(t1) => ast.Even(this(t1))
    case sast.Less(t1,t2) => ast.Less(this(t1), this(t2))
    case sast.Multiply(t1,t2) => ast.Multiply(this(t1), this(t2))
    case sast.FloorDivide(t1,t2) => ast.FloorDivide(this(t1), this(t2))
    case sast.Odd(t1) => ast.Odd(this(t1))
    case sast.Neq(t1,t2) => ast.Neq(this(t1), this(t2))
    case sast.And(t1,t2) => ast.And(this(t1), this(t2))
    case sast.Geq(t1,t2) => ast.Geq(this(t1), this(t2))
    case sast.Add(t1,t2) => ast.Add(this(t1), this(t2))
    case sast.Divide(t1,t2) => ast.Divide(this(t1), this(t2))
    case sast.Subtract(t1,t2) => ast.Add(this(t1), ast.Negate(this(t2)))
    case sast.GCD(t1,t2) => ast.GCD(this(t1), this(t2))
    case sast.LCM(t1,t2) => ast.LCM(this(t1), this(t2))
    case sast.Mod(t1,t2) => ast.Mod(this(t1), this(t2))
    case sast.Xor(t1,t2) => ast.Xor(this(t1), this(t2))
    case sast.Eq(t1,t2) => ast.Eq(this(t1), this(t2))
    case sast.Or(t1,t2) => ast.Or(this(t1), this(t2))
  }
}
