package ty

trait Ty {
  def tos () : String
  override def toString () = tos;
  def isNumeric () : Boolean;
}

object Empty extends Ty {
  def tos () = "Empty"
  def isNumeric () = false;
}

object Error extends Ty {
  def tos () = "Error"
  def isNumeric () = false;
}

object Bool extends Ty {
  def tos () = "Bool"
  def isNumeric () = false;
}

object Natural extends Ty {
  def tos () = "Natural"
  def isNumeric () = true;
}

object Integer extends Ty {
  def tos () = "Integer"
  def isNumeric () = true;
}

object Rational extends Ty {
  def tos () = "Rational"
  def isNumeric () = true;
}
