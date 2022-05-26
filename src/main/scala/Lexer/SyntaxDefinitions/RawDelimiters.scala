package Lexer.SyntaxDefinitions

object RawDelimiters extends Enumeration {
  val PLUS: RawDelimiters.Value = Value("+")
  val HYPHEN: RawDelimiters.Value = Value("-")
  val STAR: RawDelimiters.Value = Value("*")
  val FORWARD_SLASH: RawDelimiters.Value = Value("/")
  val PERCENT: RawDelimiters.Value = Value("%")
  val LEFT_ARROW: RawDelimiters.Value = Value("<")
  val RIGHT_ARROW: RawDelimiters.Value = Value(">")
  val EQUALS: RawDelimiters.Value = Value("=")
  val COLON: RawDelimiters.Value = Value(":")
  val SEMI_COLON: RawDelimiters.Value = Value(";")
  val COMMA: RawDelimiters.Value = Value(",")
  val PERIOD: RawDelimiters.Value = Value(".")
  val LEFT_PAREN: RawDelimiters.Value = Value("(")
  val RIGHT_PAREN: RawDelimiters.Value = Value(")")
  val LEFT_BRACKET: RawDelimiters.Value = Value("[")
  val RIGHT_BRACKET: RawDelimiters.Value = Value("]")
  val LEFT_BRACE: RawDelimiters.Value = Value("{")
  val RIGHT_BRACE: RawDelimiters.Value = Value("}")
  val AMPERSAND: RawDelimiters.Value = Value("&")
  val PIPE: RawDelimiters.Value = Value("|")
  val EXCLAMATION: RawDelimiters.Value = Value("!")
}
