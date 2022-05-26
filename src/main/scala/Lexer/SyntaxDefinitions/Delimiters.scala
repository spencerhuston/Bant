package Lexer.SyntaxDefinitions

object Delimiters extends Enumeration {
  val PLUS: Delimiters.Value = Value("+")
  val MINUS: Delimiters.Value = Value("-")
  val MULTIPLY: Delimiters.Value = Value("*")
  val DIVIDE: Delimiters.Value = Value("/")
  val MODULUS: Delimiters.Value = Value("%")

  val arithmeticOperators: List[Delimiters.Value] = List(PLUS, MINUS, MULTIPLY, DIVIDE)

  val LESS_THAN: Delimiters.Value = Value("<")
  val GREATER_THAN: Delimiters.Value = Value(">")
  val LESS_THAN_OR_EQUAL: Delimiters.Value = Value("<=")
  val GREATER_THAN_OR_EQUAL: Delimiters.Value = Value(">=")
  val NOT: Delimiters.Value = Value("!")
  val NOT_EQUAL: Delimiters.Value = Value("!=")
  val EQUAL: Delimiters.Value = Value("==")
  val AND: Delimiters.Value = Value("&&")
  val OR: Delimiters.Value = Value("||")

  val booleanOperators: List[Delimiters.Value] = List(LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUAL, GREATER_THAN_OR_EQUAL,
    NOT, NOT_EQUAL, EQUAL, AND, OR)

  val COLON: Delimiters.Value = Value(":")
  val SEMI_COLON: Delimiters.Value = Value(";")
  val COMMA: Delimiters.Value = Value(",")
  val ACCESS: Delimiters.Value = Value(".")
  val LEFT_PAREN: Delimiters.Value = Value("(")
  val RIGHT_PAREN: Delimiters.Value = Value(")")
  val LEFT_BRACKET: Delimiters.Value = Value("[")
  val RIGHT_BRACKET: Delimiters.Value = Value("]")
  val LEFT_BRACE: Delimiters.Value = Value("{")
  val RIGHT_BRACE: Delimiters.Value = Value("}")
  val RETURN_TYPE: Delimiters.Value = Value("->")
  val CASE_EXP: Delimiters.Value = Value("=>")
  val ASSIGNMENT: Delimiters.Value = Value("=")
  val SUBSET: Delimiters.Value = Value(":>")
  val SUPERSET: Delimiters.Value = Value("<:")
  val BIRD: Delimiters.Value = Value("|>")

  def getValue(str: String): Option[Delimiters.Value] = {
    values.find(_.toString == str) match {
      case Some(delimiterValue) => Some(delimiterValue)
      case _ => None
    }
  }
}
