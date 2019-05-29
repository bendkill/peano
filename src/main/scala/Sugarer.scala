import ast._

object Sugarer {
  def err(msg: String) : String = {println(msg); ""}

  def natToInt(t: Term) : Int = t match {
    case Zero => 0
    case Succ(t) => 1 + natToInt(t)
    case _ => println(s"$t not a natural"); 0
  }

  def integerToInt(t: Term) : Int = t match {
    case Integer(s, n) =>
      val sign : Int = s match {
        case False => 1
        case True => -1
        case _ => println("bad Integer"); 0
      }
      sign * natToInt(n)
    case _ => println(s"$t not an Integer"); 0
  }

  def apply(t: Term) : String = t match {
    case Empty => ""
    case True => "True"
    case False => "False"
    case Zero | Succ(_) => natToInt(t).toString()
    case Integer(t1, t2) => (if (t1 == True) "-" else "") ++ this(t2)
    case Rational(t1, t2) =>
      val den = natToInt(t2)
      if (den == 1)
        this(t1)
      // else if ((den % 10) == 0)
      //   (integerToInt(t1).toFloat / den.toFloat).toString
      else
        "(" ++ this(t1) ++ " / " ++ this(t2) ++ ")"
    case _ => err(s"$t is not sugarable")
  }
}
