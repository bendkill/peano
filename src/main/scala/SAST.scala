package sast

trait Term {}

object Empty extends Term {
  override def toString () = "Empty" // special term for when no terms exist
}

object True extends Term {
  override def toString () = "True"
}

object False extends Term {
  override def toString () = "False"
}

case class Natural(n:String) extends Term {
  override def toString () = s"Natural($n)"
}

case class Decimal(isNeg:Boolean, base:String, dec:String) extends Term{
  override def toString () = s"Decimal(" ++ (if (isNeg) "-" else "") ++ base ++ "." ++ dec ++ ")"
}

case class Not(t1 : Term) extends Term {
  override def toString () = s"Not($t1)"
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

