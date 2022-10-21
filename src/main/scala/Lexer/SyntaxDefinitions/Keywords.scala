package Lexer.SyntaxDefinitions

object Keywords extends Enumeration {
  val TRUE: Keywords.Value = Value("true")
  val FALSE: Keywords.Value = Value("false")
  val NULL: Keywords.Value = Value("null")
  val INT: Keywords.Value = Value("int")
  val BOOL: Keywords.Value = Value("bool")
  val CHAR: Keywords.Value = Value("char")
  val STRING: Keywords.Value = Value("string")
  val LIST: Keywords.Value = Value("List")
  val ARRAY: Keywords.Value = Value("Array")
  val SET: Keywords.Value = Value("Set")
  val TUPLE: Keywords.Value = Value("Tuple")
  val DICT: Keywords.Value = Value("Dict")
  val MATCH: Keywords.Value = Value("match")
  val CASE: Keywords.Value = Value("case")
  val ALIAS: Keywords.Value = Value("alias")
  val DERIVES: Keywords.Value = Value("derives")
  val TYPE: Keywords.Value = Value("type")
  var EXTENDS: Keywords.Value = Value("extends")
  val SEALED_TYPE: Keywords.Value = Value("sealed")
  val RECORD: Keywords.Value = Value("record")
  val TYPECLASS: Keywords.Value = Value("typeclass")
  val OF: Keywords.Value = Value("of")
  val INSTANCE: Keywords.Value = Value("instance")
  val FN: Keywords.Value = Value("fn")
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
