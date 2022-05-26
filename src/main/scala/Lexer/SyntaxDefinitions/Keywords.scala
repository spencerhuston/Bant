package Lexer.SyntaxDefinitions

object Keywords extends Enumeration {
  val TRUE: Keywords.Value = Value("true")
  val FALSE: Keywords.Value = Value("false")
  val NULL: Keywords.Value = Value("null")
  val TYPE: Keywords.Value = Value("type")
  val TYPECLASS: Keywords.Value = Value("typeclass")
  val INSTANCE: Keywords.Value = Value("instance")
  val INT: Keywords.Value = Value("int")
  val BOOL: Keywords.Value = Value("bool")
  val CHAR: Keywords.Value = Value("char")
  val STRING: Keywords.Value = Value("string")
  val LIST: Keywords.Value = Value("List")
  val ARRAY: Keywords.Value = Value("Array")
  val TUPLE: Keywords.Value = Value("Tuple")
  val DICT: Keywords.Value = Value("Dict")
  val SET: Keywords.Value = Value("Set")
  val FN: Keywords.Value = Value("fn")
  val MATCH: Keywords.Value = Value("match")
  val SWITCH: Keywords.Value = Value("switch")
  val CASE: Keywords.Value = Value("case")
  val IF: Keywords.Value = Value("if")
  val ELSE: Keywords.Value = Value("else")
  val LAZY: Keywords.Value = Value("lazy")
  val VAL: Keywords.Value = Value("val")
  val INCLUDE: Keywords.Value = Value("include")

  def getValue(str: String): Option[Keywords.Value] = {
    values.find(_.toString == str) match {
      case Some(keywordValue) => Some(keywordValue)
      case _ => None
    }
  }
}
