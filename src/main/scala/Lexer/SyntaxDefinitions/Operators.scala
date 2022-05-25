package Lexer.SyntaxDefinitions

object Operators extends Enumeration {
  val PLUS = Value("+")
  val MINUS = Value("-")
  val MULTIPLY = Value("*")
  val DIVIDE = Value("/")
  val MODULUS = Value("%")

  val arithmeticOperators: List[Operators.Value] = List(PLUS, MINUS, MULTIPLY, DIVIDE)

  val LESS_THAN = Value("<")
  val GREATER_THAN = Value(">")
  val LESS_THAN_OR_EQUAL = Value("<=")
  val GREATER_THAN_OR_EQUAL = Value(">=")
  val NOT = Value("!")
  val NOT_EQUAL = Value("!=")
  val EQUAL = Value("==")
  val AND = Value("&&")
  val OR = Value("||")

  val booleanOperators: List[Operators.Value] = List(LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUAL, GREATER_THAN_OR_EQUAL,
                          NOT, NOT_EQUAL, EQUAL, AND, OR)
}
