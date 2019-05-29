package tokens

trait Token {}

object LParen extends Token {
  override def toString () = "("
}

object RParen extends Token {
  override def toString () = ")"
}

object Minus extends Token {
  override def toString () = "-"
}

object Plus extends Token {
  override def toString () = "+"
}

object Star extends Token {
  override def toString () = "*"
}

object Slash extends Token {
  override def toString () = "/"
}

object DoubleSlash extends Token {
  override def toString () = "//"
}

object Percent extends Token {
  override def toString () = "%"
}

object Dot extends Token {
  override def toString () = "."
}


object Bang extends Token {
  override def toString () = "!"
}

object Ampersand extends Token {
  override def toString () = "&"
}

object Pipe extends Token {
  override def toString () = "|"
}

object EqualSign extends Token {
  override def toString () = "="
}

object BangEqualSign extends Token {
  override def toString () = "!="
}

object LTriangle extends Token {
  override def toString () = "<"
}

object RTriangle extends Token {
  override def toString () = ">"
}

object LTriangleEqualSign extends Token {
  override def toString () = "<="
}

object RTriangleEqualSign extends Token {
  override def toString () = ">="
}

object EvenFunc extends Token {
  override def toString () = "even"
}

object OddFunc extends Token {
  override def toString () = "odd"
}

object EqFunc extends Token {
  override def toString () = "eq"
}

object NeqFunc extends Token {
  override def toString () = "neq"
}

object AndFunc extends Token {
  override def toString () = "and"
}

object OrFunc extends Token {
  override def toString () = "or"
}

object GreaterFunc extends Token {
  override def toString () = "greater"
}

object LessFunc extends Token {
  override def toString () = "less"
}

object GeqFunc extends Token {
  override def toString () = "geq"
}

object LeqFunc extends Token {
  override def toString () = "leq"
}

object MultFunc extends Token {
  override def toString () = "mult"
}

object AddFunc extends Token {
  override def toString () = "add"
}

object DivFunc extends Token {
  override def toString () = "div"
}

object SubFunc extends Token {
  override def toString () = "sub"
}

object GCDFunc extends Token {
  override def toString () = "gcd"
}

object LCMFunc extends Token {
  override def toString () = "lcm"
}

object ModFunc extends Token {
  override def toString () = "mod"
}

object NotFunc extends Token {
  override def toString () = "not"
}

object XorFunc extends Token {
  override def toString () = "xor"
}

object FDivFunc extends Token {
  override def toString () = "fdiv"
}

object CapT extends Token {
  override def toString () = "T"
}

object CapF extends Token {
  override def toString () = "F"
}

case class Digits(x: String) extends Token {
  override def toString () = x
}


