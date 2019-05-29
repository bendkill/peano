import ast._

object TypeChecker {
  // todo: more elegantly handle type error messages?
  def err (t: Term) : ty.Ty = {println(s"type error: $t"); ty.Error}

  def atLeastInteger(tau: ty.Ty) : ty.Ty = tau match {
    case ty.Natural => ty.Integer
    case ty.Integer => ty.Integer
    case ty.Rational => ty.Rational
    case _ => err(Empty)
  }

  def atLeastRational(tau: ty.Ty) : ty.Ty = tau match {
    case ty.Natural | ty.Integer | ty.Rational => ty.Rational
    case _ => err(Empty)
  }

  def combine(tau1: ty.Ty, tau2: ty.Ty) : ty.Ty = (tau1, tau2) match {
    case (ty.Natural, tau2) => if (tau2.isNumeric) tau2 else err(Empty)
    case (ty.Integer, ty.Natural | ty.Integer) => ty.Integer
    case (ty.Rational, tau2) => if (tau2.isNumeric) ty.Rational else err(Empty)
    case _ => err(Empty)
  }

  def apply (t: Term) : ty.Ty = t match {
    case Empty => ty.Empty
    case True | False => ty.Bool
    case Zero => ty.Natural
    case Succ(t1) => if (this(t1) == ty.Natural) ty.Natural else err(t)
    case Integer(t1, t2) =>
      if (this(t1) == ty.Bool && this(t2) == ty.Natural) ty.Integer else err(t)
    case Rational(t1, t2) =>
      if (this(t1) == ty.Integer && this(t2) == ty.Natural) ty.Rational else err(t)
    case Negate(t1) => atLeastInteger(this(t1))
    case Positive(t1) => atLeastInteger(this(t1))
    case Greater(t1, t2) => t.tau = combine(this(t1), this(t2))
      if (t.tau.isNumeric) ty.Bool else err(t)
    case Even(t1) => val tau = this(t1)
      if (tau == ty.Natural || tau == ty.Integer) ty.Bool else err(t)
    case Less(t1, t2) => t.tau = combine(this(t1), this(t2))
      if (t.tau.isNumeric) ty.Bool else err(t)
    case Multiply(t1, t2) => t.tau = combine(this(t1), this(t2)); t.tau
    case FloorDivide(t1, t2) => t.tau = combine(this(t1), this(t2)); t.tau
    case Odd(t1) =>
      val tau = this(t1)
      if (tau == ty.Natural || tau == ty.Integer) ty.Bool else err(t)
    case Neq(t1, t2) =>
      val tau1 = this(t1)
      val tau2 = this(t2)
      if (tau1 == tau2) {
        t.tau = tau1
        ty.Bool
      }
      else if (tau1.isNumeric && tau2.isNumeric) {
        t.tau = combine(tau1, tau2)
        ty.Bool
      } else err(t)
    case And(t1, t2) =>
      if (this(t1) == ty.Bool && this(t2) == ty.Bool) ty.Bool else err(t)
    case Geq(t1, t2) => t.tau = combine(this(t1), this(t2))
      if (t.tau.isNumeric) ty.Bool else err(t)
    case Add(t1, t2) => t.tau = combine(this(t1), this(t2)); t.tau
    case Divide(t1, t2) =>
      t.tau = combine(this(t1), this(t2))
      if (t.tau.isNumeric) ty.Rational else err(t)
    case GCD(t1, t2) => if (this(t1) == ty.Natural && this(t2) == ty.Natural)
      ty.Natural else err(t)
    case LCM(t1, t2) => if (this(t1) == ty.Natural && this(t2) == ty.Natural)
      ty.Natural else err(t)
    case Mod(t1, t2) => t.tau = combine(this(t1), this(t2)); t.tau
    case Xor(t1, t2) =>
      if (this(t1) == ty.Bool && this(t2) == ty.Bool) ty.Bool else err(t)
    case Eq(t1, t2) =>
      val tau1 = this(t1)
      val tau2 = this(t2)
      if (tau1 == tau2) {
        t.tau = tau1
        ty.Bool
      }
      else if (tau1.isNumeric && tau2.isNumeric) {
        t.tau = combine(tau1, tau2)
        ty.Bool
      } else err(t)
    case Or(t1, t2) =>
      if (this(t1) == ty.Bool && this(t2) == ty.Bool) ty.Bool else err(t)
  }
}
