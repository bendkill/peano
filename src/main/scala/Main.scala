object Main extends App {
  println("Program execution start.")
  while (true) {
    val line = scala.io.StdIn.readLine("> ")
    if (line == "q")
      // TODO: exit gracefully
      System.exit(0)
    val toks = Scanner(line)
    val sterm = Parser(toks)
    val term = Desugarer(sterm)
    val tau = TypeChecker(term)
    if (tau != ty.Error) {
      val value = Evaluator(term)
      println(s"$value : $tau")
      println(Sugarer(value))
    }
  }
  println("Program execution end.")
}
