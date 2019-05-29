import sast._
import tokens._

object Parser {
  def err (msg: String) : Option[(Term, List[Token])] = {
    println(s"parse error: $msg")
    None
  }

  // something real funky, todo: just get constructor
  def infixToken(tok: Token) = tok match {
    case Plus => Some(Add)
    case Minus => Some(Subtract)
    case Star => Some(Multiply)
    case Slash => Some(Divide)
    case Percent => Some(Mod)
    case DoubleSlash => Some(FloorDivide)
    case Ampersand => Some(And)
    case Pipe => Some(Or)
    case EqualSign => Some(Eq)
    case BangEqualSign => Some(Neq)
    case LTriangle => Some(Less)
    case RTriangle => Some(Greater)
    case LTriangleEqualSign => Some(Leq)
    case RTriangleEqualSign => Some(Geq)
    case _ => None
  }

  // returns the number of terms to take after the prefix.
  // 0 means grab first available subTerm (greedily)
  def prefixToken(tok: Token, ts: List[Term]) = (tok, ts) match {
    case (Bang, t1 :: Nil) => Some(Not(t1))
    case (Minus, t1 :: Nil) => Some(Negate(t1))
    case (Plus, t1 :: Nil) => Some(Positive(t1))
    case (GreaterFunc, t1 :: t2 :: Nil) => Some(Greater(t1, t2))
    case (EvenFunc, t1 :: Nil) => Some(Even(t1))
    case (LessFunc, t1 :: t2 :: Nil) => Some(Less(t1, t2))
    case (MultFunc, t1 :: t2 :: Nil) => Some(Multiply(t1, t2))
    case (FDivFunc, t1 :: t2 :: Nil) => Some(FloorDivide(t1, t2))
    case (NotFunc, t1 :: Nil) => Some(Not(t1))
    case (OddFunc, t1 :: Nil) => Some(Odd(t1))
    case (NeqFunc, t1 :: t2 :: Nil) => Some(Neq(t1, t2))
    case (AndFunc, t1 :: t2 :: Nil) => Some(And(t1, t2))
    case (GeqFunc, t1 :: t2 :: Nil) => Some(Geq(t1, t2))
    case (LeqFunc, t1 :: t2 :: Nil) => Some(Leq(t1, t2))
    case (AddFunc, t1 :: t2 :: Nil) => Some(Add(t1, t2))
    case (DivFunc, t1 :: t2 :: Nil) => Some(Divide(t1, t2))
    case (SubFunc, t1 :: t2 :: Nil) => Some(Subtract(t1, t2))
    case (GCDFunc, t1 :: t2 :: Nil) => Some(GCD(t1, t2))
    case (LCMFunc, t1 :: t2 :: Nil) => Some(LCM(t1, t2))
    case (ModFunc, t1 :: t2 :: Nil) => Some(Mod(t1, t2))
    case (XorFunc, t1 :: t2 :: Nil) => Some(Xor(t1, t2))
    case (EqFunc, t1 :: t2 :: Nil) => Some(Eq(t1, t2))
    case (OrFunc, t1 :: t2 :: Nil) => Some(Or(t1, t2))
    case _ => None
  }

  def prefixTokenArgs(tok: Token) = tok match {
    case Bang | Minus | Plus => Some(0)
    case EvenFunc | NotFunc | OddFunc => Some(1)
    case (GreaterFunc | LessFunc | MultFunc | FDivFunc | NeqFunc | AndFunc |
        GeqFunc | LeqFunc | AddFunc | DivFunc | SubFunc | GCDFunc | LCMFunc |
        ModFunc | XorFunc | EqFunc | OrFunc) => Some(2)
    case _ => None
  }

  def postFix (t1: Term, toks: List[Token]) : Option[(Term, List[Token])] =
    toks match {
      case tok :: toks => infixToken(tok) match {
        case Some(constructor) => nextSubTerm(toks) match {
          case Some((t2, toks)) => postFix(constructor(t1, t2), toks)
          case None => err(s"hanging $tok")
        }
        case None => Some((t1, tok::toks))
      }
      case _ => Some((t1, toks))
    }

  def nextSubTerm (toks : List[Token]) : Option[(Term, List[Token])] = toks match {
    case CapT :: toks => Some((True, toks))
    case CapF :: toks => Some((False, toks))
    case Dot :: Digits(s) :: toks =>
      Some((Decimal(false, "0", s), toks))
    case Digits(b) :: Dot :: Digits(d) :: toks =>
      Some((Decimal(false, b, d), toks))
    case Digits(b) :: Dot :: toks =>
      Some((Decimal(false, b, "0"), toks))
    case Digits(s) :: toks => Some((Natural(s), toks))
    case LParen :: toks => nextTerm(toks) match {
      case Some((term, RParen :: toks)) => Some((term, toks))
      case Some((term, _)) => err("hanging (")
      case None => err("hanging (")
    }
    case _ => err("expected term")
  }

  def takeNTerms(n : Int, toks : List[Token]) : Option[(List[Term], List[Token])] =
    if (n == 0) Some(Nil, toks)
    else nextTerm(toks) match {
      case Some((term, toks)) => takeNTerms(n - 1, toks) match {
        case Some((terms, ts)) => Some (term :: terms, ts)
        case None => None
      }
      case None => None
    }

  def nextTerm (toks : List[Token]) : Option[(Term, List[Token])] = toks match {
    case Nil => None
    case tok :: Nil => nextSubTerm(toks) match {
      case Some((t1, Nil)) => Some (t1, Nil)
      case Some(_) => throw new RuntimeException("catastrophe")
      case None => None
    }
    case tok :: toks => prefixTokenArgs(tok) match {
      case Some(0) => nextSubTerm(toks) match {
        case Some((t1, toks)) => prefixToken(tok, t1 :: Nil) match {
          case Some(term) => postFix(term, toks)
          case None => None
        }
        case None => None
      }
      case Some(n) => takeNTerms(n, toks) match {
        case Some((ts, toks)) => prefixToken(tok, ts) match {
          case Some(term) => postFix(term, toks)
          case None => None
        }
        case None => None
      }
      case None => nextSubTerm(tok :: toks) match {
        case Some((term, toks)) => postFix(term, toks)
        case None => None
      }
    }
  }

  def apply (toks : List[Token]) : Term = nextTerm(toks) match {
    case Some((term, Nil)) => term
    case Some((term, toks)) => println(s"too many toks? ($term, $toks)"); Empty
    case None => Empty
  }
}
