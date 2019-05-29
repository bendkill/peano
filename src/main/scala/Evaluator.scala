import ast._

object Evaluator {

  def err (msg: String) : Term = {
    println(s"eval error: $msg")
    Empty
  }

  def castErr (tau: ty.Ty) : Term = err(s"cast error: $tau")
  def termErr (t: Term) : Term = err(t.toString)
  def typeErr (s: String) : Term = err("unchecked type: " ++ s)

  def cast (t: Term, tau:ty.Ty) : Term = tau match {
    case ty.Bool => t match {
      case True | False => t
      case _ => castErr(tau)
    }
    case ty.Natural => t match {
      case Zero | Succ(_) => t
      case Integer(False | True, Zero) => Zero
      case Integer(False, t2) => t2
      case Rational(Integer(False | True, Zero), _) => Zero
      case Rational(Integer(False, t2), Succ(Zero)) => t2
      case _ => castErr(tau)
    }
    case ty.Integer => t match {
      case Zero | Succ(_) => Integer(False, t)
      case Integer(_, _) => t
      case Rational(t1, Succ(Zero)) => t1
      case _ => castErr(tau)
    }
    case ty.Rational => t match {
      case Zero | Succ(_) => Rational(Integer(False, t), Succ(Zero))
      case Integer(_, _) => Rational(t, Succ(Zero))
      case Rational(_, _) => t
      case _ => castErr(tau)
    }
    case _ => castErr(tau)
  }

  def negate (v: Term) : Term = v match {
    case Zero => Integer(False, Zero)
    case Succ(v1) => Integer(True, Succ(v1))
    case Integer(False, v1) => Integer(True, v1)
    case Integer(True, v1) => Integer(False, v1)
    case Rational(Integer(False, v1), v2) => Rational(Integer(True, v1), v2)
    case Rational(Integer(True, v1), v2) => Rational(Integer(False, v1), v2)
    case _ => termErr(v)
  }

  def positive (v:Term) : Term = v match {
    case Zero => Integer(False, Zero)
    case Succ(v1) => Integer(False, Succ(v1))
    case Integer(v1, v2) => Integer(v1, v2)
    case Rational(v1, v2) => Rational(v1, v2)
    case _ => termErr(v)
  }

  def not(v: Term) : Term = v match {
    case True => False
    case False => True
    case _ => typeErr("not")
  }

  def or(v1: Term, v2: Term) = v1 match {
    case True => True
    case False => v2
    case _ => typeErr("or")
  }

  def and(v1: Term, v2: Term) = v1 match {
    case True => v2
    case False => False
    case _ => typeErr("and")
  }

  def xor(v1: Term, v2: Term) = v1 match {
    case False => v2
    case True => not(v2)
    case _ => typeErr("xor")
  }

  def even(v: Term) : Term = v match {
    case Zero => True
    case Succ(Zero) => False
    case Succ(v1) => not(even(v1))
    case Integer(_, v2) => even(v2)
    case Rational(_, _) => err(s"even of Rational")
    case _ => typeErr("even")
  }

  def odd(v: Term) : Term = not(even(v))

  def equal(v1: Term, v2: Term) : Term = (v1, v2) match {
    case (True, True) => True
    case (True, False) => False
    case (False, True) => False
    case (False, False) => True
    case (Zero, Zero) => True
    case (Succ(_), Zero) => False
    case (Zero, Succ(_)) => False
    case (Succ(v1), Succ(v2)) => equal(v1, v2)
    case (Integer(s1, v1), Integer(s2, v2)) => and(equal(s1, s2), equal(v1, v2))
    case (Rational(n1, d1), Rational(n2, d2)) => err("todo: reduce rationals")
    case _ => typeErr("equal")
  }

  def neq(v1: Term, v2: Term) : Term = not(equal(v1, v2))

  def greater(v1: Term, v2: Term) : Term = (v1, v2) match {
    case (Zero, Zero | Succ(_)) => False
    case (Succ(_), Zero) => True
    case (Succ(v1), Succ(v2)) => greater(v1, v2)
    case (Integer(_, Zero), Integer(_, Zero)) => False
    case (Integer(_, Zero), Integer(s2, _)) => s2
    case (Integer(s1, _), Integer(_, Zero)) => not(s1)
    case (Integer(False, v1), Integer(False, v2)) => greater(v1, v2)
    case (Integer(False, v1), Integer(True, v2)) => True
    case (Integer(True, v1), Integer(False, v2)) => False
    case (Integer(True, v1), Integer(True, v2)) => greater(v2, v1)
    case (Rational(n1, d1), Rational(n2, d2)) =>
      val g = cast(gcd(d1, d2), ty.Integer)
      greater(mult(n1, g), mult(n2, g))
    case _ => typeErr("greater")
  }

  def geq(v1: Term, v2: Term) : Term = (v1, v2) match {
    case (Zero | Succ(_), Zero) => True
    case (Zero, Succ(_)) => False
    case (Succ(v1), Succ(v2)) => geq(v1, v2)
    case (Integer(_, Zero), Integer(_, Zero)) => True
    case (Integer(_, Zero), Integer(s2, _)) => s2
    case (Integer(s1, _), Integer(_, Zero)) => not(s1)
    case (Integer(False, v1), Integer(False, v2)) => geq(v1, v2)
    case (Integer(False, v1), Integer(True, v2)) => True
    case (Integer(True, v1), Integer(False, v2)) => False
    case (Integer(True, v1), Integer(True, v2)) => geq(v2, v1)
    case (Rational(n1, d1), Rational(n2, d2)) =>
      val g = cast(gcd(d1, d2), ty.Integer)
      geq(mult(n1, g), mult(n2, g))
    case _ => typeErr(s"geq: $v1, $v2")
  }

  def less(v1: Term, v2: Term) : Term = greater(v2, v1)

  def leq(v1: Term, v2: Term) : Term = geq(v2, v1)

  // Quotient, Remainder : (Natural, Natural)
  def divByTwoNat(v: Term) : (Term, Term) = v match {
    case Zero => (Zero, Zero)
    case Succ(Zero) => (Zero, Succ(Zero))
    case Succ(Succ(v1)) =>
      val (q, r) = divByTwoNat(v1)
      (Succ(q), r)
    case _ => (typeErr(s"divByTwoNat: $v"), Empty)
  }

  def multByTwoNat(v: Term) : Term = v match {
    case Zero => Zero
    case Succ(v1) => Succ(Succ(multByTwoNat(v1)))
    case _ => typeErr(s"multByTwoNat: $v")
  }

  def mult(v1: Term, v2: Term) : Term = (v1, v2) match {
    case (Zero, _) => Zero
    case (Succ(Zero), v2) => v2
    case (Succ(v1), v2) => add(mult(v1, v2), v2)
    case (Integer(s1, v1), Integer(s2, v2)) => Integer(xor(s1, s2), mult(v1, v2))
    case (Rational(n1, d1), Rational(n2, d2)) => Rational(mult(n1, n2), mult(d1, d2))
    case _ => typeErr(s"mult: $v1, $v2")
  }

  // v1 - v2, but returns Empty if v2 > v1
  def subNat(v1: Term, v2: Term) : Term = (v1, v2) match {
    case (Zero, Zero) => Zero
    case (Zero, Succ(_)) => Empty
    case (Succ(_), Zero) => v1
    case (Succ(v1), Succ(v2)) => subNat(v1, v2)
    case _ => typeErr("subNat")
  }

  // quotient, remainder
  def divNat(v1: Term, v2: Term) : (Term, Term) = (v1, v2) match {
    case (_, Zero) => (err("divide by zero"), Empty)
    case (Zero, _) => (Zero, v2)
    case (Succ(_), Succ(_)) => subNat(v1, v2) match {
      case Empty => (Zero, v1)
      case Zero => (Succ(Zero), Zero)
      case Succ(v1) =>
        val (q, r) = divNat(Succ(v1), v2)
        (Succ(q), r)
      case _ => (Empty, Empty)
    }
    case _ => (typeErr("divNat"), Empty)
  }

  def div(v1: Term, v2: Term) : Term = (v1, v2) match {
    case (Zero | Succ(_), Zero | Succ(_)) =>
      divNat(v1, v2) match {
        case (Zero, r) => Rational(Integer(False, v1), v2)
        case (q, Zero) => Rational(Integer(False, q), Succ(Zero))
        case (q, r) => add(
          Rational(Integer(False, q), Succ(Zero)),
          Rational(Integer(False, r), v2))
      }
    case (Integer(s1, n1), Integer(s2, n2)) => div(n1, n2) match {
      case Rational(Integer(_, n), d) => Rational(Integer(xor(s1, s2), n), d)
      case _ => typeErr(s"div: $v1, $v2")
    }
    case (Rational(n1, d1), Rational(Integer(s2, n2), d2)) =>
      mult(Rational(n1, d1), Rational(Integer(s2, d2), n2))
    case (_, _) => typeErr(s"div: $v1, $v2")
  }

  def fulldiv(v1: Term, v2: Term) : (Term, Term) = (v1, v2) match {
    case (Zero | Succ(_), Zero | Succ(_)) => divNat(v1, v2)
    case (Integer(s1, n1), Integer(s2, n2)) =>
      val (q, r) = divNat(n1, n2)
      if (q == Empty || r == Empty) (Empty, Empty)
      else (Integer(xor(s1, s2), q), Integer(s2, r))
    case (Rational(n1, d1), Rational(Integer(s2, n2), d2)) =>
      reduce(mult(Rational(n1, d1), Rational(Integer(s2, d2), n2))) match {
        case Rational(Integer(s, n), d) =>
          val (q, r) = fulldiv(Integer(s,n), Integer(False, d))
          (cast(q, ty.Rational), cast(r, ty.Rational))
        case _ => (typeErr(s"multiplication of rationals"), Empty)
      }
    case (_, _) => (typeErr(s"div: $v1, $v2"), Empty)
  }

  def fdiv(v1: Term, v2: Term) : Term = { val (q,_) = fulldiv(v1, v2); q }

  def mod(v1: Term, v2: Term) : Term = { val (_,r) = fulldiv(v1, v2); r }

  // should always return a natural
  def gcd(v1: Term, v2: Term) : Term = if (v1 == Zero) v2
    else (divByTwoNat(v1), divByTwoNat(v2)) match {
      case ((q1, Zero), (q2, Zero)) => multByTwoNat(gcd(q1, q2))
      case ((q1, Zero), (q2, Succ(Zero))) => gcd(q1, v2)
      case ((q1, Succ(Zero)), (q2, Zero)) => gcd(v1, q2)
      case ((q1, Succ(Zero)), (q2, Succ(Zero))) => geq(q1, q2) match {
        case True =>
          val (q, _) = divByTwoNat(subNat(v1, v2))
          gcd(q, v2)
        case False =>
          val (q, _) = divByTwoNat(subNat(v2, v1))
          gcd(q, v1)
        case _ => typeErr("geq")
      }
      case _ => typeErr("gcd")
    }

  // should both be naturals, returns a natural
  def lcm(v1: Term, v2: Term) : Term =
    cast(div(mult(v1, v2), gcd(v1, v2)), ty.Natural)

  // should only be called when necessary
  def reduce(t: Term) : Term = t match {
    case Zero | Succ(_) => t
    case Integer(_, _) => t
    case Rational(Integer(s, n), d) =>
      val g = gcd(n,d)
      Rational(Integer(s, cast(div(n,g), ty.Natural)), cast(div(d,g), ty.Natural))
  }

  def add(v1: Term, v2: Term) : Term = (v1, v2) match {
    case (Zero, v2) => v2
    case (Succ(v1), v2) => add(v1, Succ(v2))

    case (Integer(True | False, Zero), v2) => v2
    case (v1, Integer(True | False, Zero)) => v1
    case (Integer(False, v1), Integer(False, v2)) => Integer(False, add(v1, v2))
    case (Integer(True, v1), Integer(True, v2)) => Integer(True, add(v1, v2))
    case (Integer(False, Succ(v1)), Integer(True, Succ(v2))) =>
      add(Integer(False, v1), Integer(True, v2))
    case (Integer(True, Succ(v1)), Integer(False, Succ(v2))) =>
      add(Integer(True, v1), Integer(False, v2))

    case (Rational(Integer(_, Zero), _), v2) => v2
    case (v1, Rational(Integer(_, Zero), _)) => v1
    case (Rational(n1, d1), Rational(n2, d2)) =>
      // println(s"add: ($d1, $d2) + ($d1, $d2)")
      val d = lcm(d1, d2) // should be a natural
      val n = add(
        mult(n1, cast(div(d, d1), ty.Integer)),
        mult(n2, cast(div(d, d2), ty.Integer)))
      Rational(n, d)
    case _ => err("unchecked type")
  }

  def apply (t: Term) : Term = t match {
    case Empty => Empty
    case True => True
    case False => False
    case Zero => Zero
    case Succ(t1) => Succ(this(t1))
    case Integer(t1, t2) => Integer(this(t1), this(t2))
    case Rational(t1, t2) => reduce(Rational(this(t1), this(t2)))
    case Negate(t1) => negate(this(t1))
    case Positive(t1) => positive(this(t1))
    case Greater(t1, t2) => greater(cast(this(t1), t.tau), cast(this(t2), t.tau))
    case Even(t1) => even(this(t1))
    case Less(t1, t2) => less(cast(this(t1), t.tau), cast(this(t2), t.tau))
    case Multiply(t1, t2) =>
      reduce(mult(cast(this(t1), t.tau), cast(this(t2), t.tau)))
    case FloorDivide(t1, t2) =>
      reduce(fdiv(cast(this(t1), t.tau), cast(this(t2), t.tau)))
    case Odd(t1) => odd(this(t1))
    case Neq(t1, t2) => neq(cast(this(t1), t.tau), cast(this(t2), t.tau))
    case And(t1, t2) => and(this(t1), this(t2))
    case Geq(t1, t2) => geq(cast(this(t1), t.tau), cast(this(t2), t.tau))
    case Add(t1, t2) =>
      reduce(add(cast(this(t1), t.tau), cast(this(t2), t.tau)))
    case Divide(t1, t2) =>
      reduce(div(cast(this(t1), t.tau), cast(this(t2), t.tau)))
    case GCD(t1, t2) => gcd(this(t1), this(t2))
    case LCM(t1, t2) => lcm(this(t1), this(t2))
    case Mod(t1, t2) =>
      reduce(mod(cast(this(t1), t.tau), cast(this(t2), t.tau)))
    case Xor(t1, t2) => xor(this(t1), this(t2))
    case Eq(t1, t2) => equal(cast(this(t1), t.tau), cast(this(t2), t.tau))
    case Or(t1, t2) => or(this(t1), this(t2))
  }
}
