package ast

trait Term {
  var tau : ty.Ty = ty.Empty
  def withType (tau1: ty.Ty) = {tau = tau1; this}
  def isNat () : Boolean = false;
  def isVal () : Boolean = false;
}

object Empty extends Term {
  override def toString () = "Empty"
  override def isVal () = true
}

object True extends Term {
  override def toString () = "T"
  override def isVal () = true
}

object False extends Term {
  override def toString () = "F"
  override def isVal () = true
}

object Zero extends Term {
  override def toString () = "Z"
  override def isNat () = true
  override def isVal () = true
}

case class Succ(t1: Term) extends Term {
  override def toString () = s"S$t1"
  override def isNat () = t1.isNat()
  override def isVal () = t1.isNat()
}

case class Integer(t1: Term, t2: Term) extends Term {
  // t1 : Bool, t2 natural
  override def toString () = {
    val sign = if (t1 == True) "-" else "+"
    s"$sign$t2"
  }
  override def isVal () = ((t1 == True) || (t1 == False)) && t2.isVal()
}

case class Rational(t1 : Term, t2: Term) extends Term {
  // t1: integer, t2: natural
  override def toString () = s"($t1 / $t2)"
  override def isVal () = t1.isVal() && t2.isVal() // improper rationals still values
}

case class Negate(t1 : Term) extends Term {
  override def toString () = s"Negate($t1)"
}

case class Positive(t1 : Term) extends Term {
  override def toString () = s"Positive($t1)"
}

case class Greater(t1 : Term, t2: Term) extends Term {
  override def toString () = s"Greater($t1, $t2)"
}

case class Even(t1 : Term) extends Term {
  override def toString () = s"Even($t1)"
}

case class Less(t1 : Term, t2: Term) extends Term {
  override def toString () = s"Less($t1, $t2)"
}

case class Multiply(t1: Term, t2: Term) extends Term {
  override def toString () = s"Multiply($t1, $t2)"
}

case class FloorDivide(t1: Term, t2: Term) extends Term {
  override def toString () = s"FloorDivide($t1, $t2)"
}

case class Odd(t1: Term) extends Term {
  override def toString () = s"Odd($t1)"
}

case class Neq(t1: Term, t2: Term) extends Term {
  override def toString () = s"Neq($t1)"
}

case class And(t1: Term, t2: Term) extends Term {
  override def toString () = s"And($t1)"
}

case class Geq(t1: Term, t2: Term) extends Term {
  override def toString () = s"Geq($t1, $t2)"
}

case class Leq(t1: Term, t2: Term) extends Term {
  override def toString () = s"Leq($t1, $t2)"
}

case class Add(t1 : Term, t2 : Term) extends Term {
  override def toString () = s"Add($t1, $t2)"
}

case class Divide(t1: Term, t2: Term) extends Term {
  override def toString () = s"Divide($t1, $t2)"
}

case class Subtract(t1 : Term, t2 : Term) extends Term {
  override def toString () = s"Subtract($t1, $t2)"
}

case class GCD(t1: Term, t2: Term) extends Term {
  override def toString () = s"GCD($t1, $t2)"
}

case class LCM(t1: Term, t2: Term) extends Term {
  override def toString () = s"LCM($t1, $t2)"
}

case class Mod(t1: Term, t2: Term) extends Term {
  override def toString () = s"Mod($t1, $t2)"
}

case class Xor(t1: Term, t2: Term) extends Term {
  override def toString () = s"Xor($t1, $t2)"
}

case class Eq(t1: Term, t2: Term) extends Term {
  override def toString () = s"Eq($t1, $t2)"
}

case class Or(t1: Term, t2: Term) extends Term {
  override def toString () = s"Or($t1, $t2)"
}
