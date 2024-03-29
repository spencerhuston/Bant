package Lexer.SyntaxDefinitions

object Delimiters extends Enumeration {
  val PLUS: Delimiters.Value = Value("+")
  val MINUS: Delimiters.Value = Value("-")
  val MULTIPLY: Delimiters.Value = Value("*")
  val DIVIDE: Delimiters.Value = Value("/")
  val MODULUS: Delimiters.Value = Value("%")

  val arithmeticOperators: Delimiters.ValueSet = ValueSet(PLUS, MINUS, MULTIPLY, DIVIDE, MODULUS)

  val LESS_THAN: Delimiters.Value = Value("<")
  val GREATER_THAN: Delimiters.Value = Value(">")
  val LESS_THAN_OR_EQUAL: Delimiters.Value = Value("<=")
  val GREATER_THAN_OR_EQUAL: Delimiters.Value = Value(">=")
  val NOT: Delimiters.Value = Value("!")
  val NOT_EQUAL: Delimiters.Value = Value("!=")
  val EQUAL: Delimiters.Value = Value("==")
  val AND: Delimiters.Value = Value("&&")
  val OR: Delimiters.Value = Value("||")

  val booleanOperators: Delimiters.ValueSet = ValueSet(LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUAL, GREATER_THAN_OR_EQUAL,
    NOT, NOT_EQUAL, EQUAL, AND, OR)

  val COLON: Delimiters.Value = Value(":")
  val STATEMENT_END: Delimiters.Value = Value(";")
  val COMMA: Delimiters.Value = Value(",")
  val TUPLE_ACCESS: Delimiters.Value = Value(".")
  val RECORD_ACCESS: Delimiters.Value = Value("::")
  val LEFT_PAREN: Delimiters.Value = Value("(")
  val RIGHT_PAREN: Delimiters.Value = Value(")")
  val LEFT_BRACKET: Delimiters.Value = Value("[")
  val RIGHT_BRACKET: Delimiters.Value = Value("]")
  val LEFT_BRACE: Delimiters.Value = Value("{")
  val RIGHT_BRACE: Delimiters.Value = Value("}")
  val RETURN_TYPE: Delimiters.Value = Value("->")
  val CASE_EXP: Delimiters.Value = Value("=>")
  val ASSIGNMENT: Delimiters.Value = Value("=")
  val LOWER_BOUND: Delimiters.Value = Value(":>")
  val UPPER_BOUND: Delimiters.Value = Value("<:")
  val FUNC_CHAIN: Delimiters.Value = Value("|>")
  val LAMBDA: Delimiters.Value = Value("|")

  def getValue(str: String): Option[Delimiters.Value] = {
    values.find(_.toString == str) match {
      case Some(delimiterValue) => Some(delimiterValue)
      case _ => None
    }
  }
}
