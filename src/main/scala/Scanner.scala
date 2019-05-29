import tokens._

object Scanner {
  def err(msg: String) = println(s"scan error: $msg")

  def nextToken (cs: List[Char]) : Option[(Token, List[Char])] = cs match {
    case (' ' | '\t') :: cs => nextToken(cs)

    case 'g' :: 'r' :: 'e' :: 'a' :: 't' :: 'e' :: 'r' :: cs => Some (GreaterFunc, cs)
    case 'e' :: 'v' :: 'e' :: 'n' :: cs => Some (EvenFunc, cs)
    case 'l' :: 'e' :: 's' :: 's' :: cs => Some (LessFunc, cs)
    case 'm' :: 'u' :: 'l' :: 't' :: cs => Some (MultFunc, cs)
    case 'f' :: 'd' :: 'i' :: 'v' :: cs => Some (FDivFunc, cs)
    case 'n' :: 'o' :: 't' :: cs => Some (NotFunc, cs)
    case 'o' :: 'd' :: 'd' :: cs => Some (OddFunc, cs)
    case 'n' :: 'e' :: 'q' :: cs => Some (NeqFunc, cs)
    case 'a' :: 'n' :: 'd' :: cs => Some (AndFunc, cs)
    case 'g' :: 'e' :: 'q' :: cs => Some (GeqFunc, cs)
    case 'l' :: 'e' :: 'q' :: cs => Some (LeqFunc, cs)
    case 'a' :: 'd' :: 'd' :: cs => Some (AddFunc, cs)
    case 'd' :: 'i' :: 'v' :: cs => Some (DivFunc, cs)
    case 's' :: 'u' :: 'b' :: cs => Some (SubFunc, cs)
    case 'g' :: 'c' :: 'd' :: cs => Some (GCDFunc, cs)
    case 'l' :: 'c' :: 'm' :: cs => Some (LCMFunc, cs)
    case 'm' :: 'o' :: 'd' :: cs => Some (ModFunc, cs)
    case 'x' :: 'o' :: 'r' :: cs => Some (XorFunc, cs)
    case 'e' :: 'q' :: cs => Some (EqFunc, cs)
    case 'o' :: 'r' :: cs => Some (OrFunc, cs)

    case '!' :: '=' :: cs => Some (BangEqualSign, cs)
    case '/' :: '/' :: cs => Some (DoubleSlash, cs)
    case '<' :: '=' :: cs => Some (LTriangleEqualSign, cs)
    case '>' :: '=' :: cs => Some (RTriangleEqualSign, cs)

    case '(' :: cs => Some (LParen, cs)
    case ')' :: cs => Some (RParen, cs)
    case '-' :: cs => Some (Minus, cs)
    case '+' :: cs => Some (Plus, cs)
    case '*' :: cs => Some (Star, cs)
    case '/' :: cs => Some (Slash, cs)
    case '.' :: cs => Some (Dot, cs) // TODO: disallow "123 . 456"
    case '%' :: cs => Some (Percent, cs)
    case '!' :: cs => Some (Bang, cs)
    case '&' :: cs => Some (Ampersand, cs)
    case '|' :: cs => Some (Pipe, cs)
    case '=' :: cs => Some (EqualSign, cs)
    case '<' :: cs => Some (LTriangle, cs)
    case '>' :: cs => Some (RTriangle, cs)
    case 'T' :: cs => Some (CapT, cs)
    case 'F' :: cs => Some (CapF, cs)

    case c :: cs =>
      if (c.isDigit) {
        val (xs, rs) = (c::cs).span(x => x.isDigit)
        Some (Digits(xs.mkString), rs)
      } else {
        err(c.toString)
        None
      }
    case Nil => None
  }

  def apply (s:String) : List[Token] = {
    def lp (cs : List[Char]) : List[Token] = cs match {
      case Nil => Nil
      case cs => nextToken(cs) match {
        case Some((tok, cs)) => tok :: lp(cs)
        case None => Nil
      }
    }
    lp(s.toList)
  }
}
