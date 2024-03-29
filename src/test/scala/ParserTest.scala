import Lexer.SyntaxDefinitions.Delimiters
import Lexer.SyntaxDefinitions.Delimiters._
import Lexer.SyntaxDefinitions.Keywords._
import Lexer.{Delimiter, EOF, FilePosition, Ident, Keyword, Token}
import Logger.Logger.lineList
import Parser.{Alias, AnyCase, ArrayDef, BoolVal, Branch, Case, ConstructorCase, DictDef, FuncApp, IntVal, Let, ListDef, Lit, LitCase, Match, NoOp, NullVal, Prim, Prog, Ref, SetDef, StringVal, TupleAccess, TupleDef, TypeCase, ValueCase}
import Parser.Parser._
import SemanticAnalyzer._

import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer

class ParserTest extends AnyFlatSpec {
  def clear(): Unit = {
    index = 0
    tokenStream = ArrayBuffer[Token]()
    numErrors = 0
    warnings = 0
    dummyCount = 0
    anonCount = 0
    lineList = Array[String]()
    BantRunner.Main.clear()
  }

  def fp: FilePosition = FilePosition(0, 0, "val")

  def getExp(path: String): Parser.Exp = {
    clear()
    val ts = BantRunner.Main.getSource(Array[String]("-f", path)) match {
      case Some(source) =>
        Lexer.Lexer.scan(source)
    }
    parse(ts)
  }

  "Parser.curr" must "give the current token" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(curr.tokenText == "val")
  }

  it should "fail if out of bounds" in {
    index = 1
    assertThrows[IndexOutOfBoundsException] {
      curr.tokenText == "val"
    }
  }

  "Parser.advance" must "advance the index" in {
    clear()
    index = 0
    advance()
    assert(index == 1)
  }

  "Parser.none" must "return a no-op expression" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(none.token.tokenText == "val")
  }

  "Parser.isEof" must "return true if curr is EOF" in {
    clear()
    tokenStream = ArrayBuffer[Token](EOF("EOF", fp))
    index = 0
    assert(isEof)
  }

  it should "return false if curr is not EOF" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(!isEof)
  }

  "Parser.matchStatementEndRequired" should "report bad match if not terminator" in {
    clear()
    tokenStream = Lexer.Lexer.scan("val")
    index = 0
    matchStatementEndRequired()
    assert(index == 0 &&
           Parser.Parser.numErrors > 0)
  }

  "Parser.matchRequired" must "return true if match is made and advance" in {
    clear()
    tokenStream = Lexer.Lexer.scan("val")
    index = 0
    assert(matchRequired(VAL) && index == 1)
  }

  it should "return false if match is not made" in {
    clear()
    tokenStream = Lexer.Lexer.scan("val")
    index = 0
    assert(!matchRequired(LAZY) && index == 1 && numErrors > 0)
  }

  "Parser.matchOptional" must "return true if match is made and advance" in {
    clear()
    tokenStream = Lexer.Lexer.scan("val")
    index = 0
    assert(matchOptional(VAL) && index == 1)
  }

  it should "return false if match is not made and not advance" in {
    clear()
    tokenStream = Lexer.Lexer.scan("val")
    index = 0
    assert(!matchOptional(LAZY) && index == 0 && numErrors == 0)
  }

  "Parser.peek" should "return true if next value matches" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp), Keyword(LAZY, "lazy", fp))
    index = 0
    assert(peek(LAZY))
  }

  it should "return false if next value does not match" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp), Keyword(LAZY, "lazy", fp))
    index = 0
    assert(!peek(VAL))
  }

  it should "return false curr is last token" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(!peek(LAZY))
  }

  "Parser.matchIdent" should "return ident if curr is one" in {
    clear()
    tokenStream = ArrayBuffer[Token](Ident("name", fp))
    index = 0
    assert(matchIdent == "name" && index == 1)
  }

  it should "return empty string if curr is not ident" in {
    clear()
    lineList = Array[String]("val")
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(matchIdent != "name" && index == 1)
  }

  "Parser.isBinaryOperator" must "return true if op is arith or boolean binary op" in {
    clear()
    tokenStream = ArrayBuffer[Token](Delimiter(PLUS, "+", fp))
    index = 0
    assert(isBinaryOperator(0))
  }

  it must "return false if op is not arith or boolean binary op" in {
    clear()
    tokenStream = ArrayBuffer[Token](Delimiter(LAMBDA, "|", fp))
    index = 0
    assert(!isBinaryOperator(0))
  }

  it must "return false if op min is less than curr op precedence" in {
    clear()
    tokenStream = ArrayBuffer[Token](Delimiter(PLUS, "+", fp))
    index = 0
    assert(!isBinaryOperator(5))
  }

  "Parser.getPrecedence" must "return corresponding value for each op" in {
    clear()
    assert(getPrecedence(AND) == 0 && getPrecedence(OR) == 0)
    assert(getPrecedence(PLUS) == 2 && getPrecedence(MINUS) == 2)
    assert(getPrecedence(MULTIPLY) == 3 &&
      getPrecedence(DIVIDE) == 3 &&
      getPrecedence(MODULUS) == 3)
    assert(getPrecedence(GREATER_THAN) == 1)
  }

  "Parser.dummy" must "return dummy name" in {
    clear()
    assert(dummy == "dummy$0")
  }

  "Parser.anon" must "return anon name" in {
    clear()
    assert(anon == "anon$0")
  }

  "Parser.parse" must "catch OOB exception" in {
    clear()
    tokenStream = Lexer.Lexer.scan("lazy")
    index = 3
    val exp = parse(tokenStream)
    assert(exp.isInstanceOf[NoOp])
  }

  "Parser.parseExp" must "return simplest let when given tokens" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/let_test.bnt")
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(!let.isLazy)
    assert(let.ident == "a")
    assert(let.letType == UnknownType())
    assert(let.expType == UnknownType())
    assert(let.expValue.isInstanceOf[Lit])
    assert(let.expValue.asInstanceOf[Lit].value == IntVal(0))
    assert(let.afterLet.isInstanceOf[NoOp])
  }

  it must "return let with annotations" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/let_test2.bnt")
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(let.isLazy)
    assert(let.ident == "a")
    assert(let.letType == IntType())
    assert(let.expType == UnknownType())
    assert(let.expValue.isInstanceOf[Lit])
    assert(let.expValue.asInstanceOf[Lit].value == IntVal(0))
    assert(let.afterLet.isInstanceOf[Ref])
    assert(let.afterLet.asInstanceOf[Ref].ident == "a")
  }

  it must "return nested let in let" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/let_test3.bnt")
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(!let.isLazy)
    assert(let.ident == "a")
    assert(let.letType == UnknownType())
    assert(let.expType == UnknownType())
    assert(let.expValue.isInstanceOf[Lit])
    assert(let.expValue.asInstanceOf[Lit].value == IntVal(5))
    assert(let.afterLet.isInstanceOf[Let])

    val let2 = let.afterLet.asInstanceOf[Let]
    assert(!let2.isLazy)
    assert(let2.ident == "b")
    assert(let2.letType == UnknownType())
    assert(let2.expType == UnknownType())
    assert(let2.expValue.isInstanceOf[Lit])
    assert(let2.expValue.asInstanceOf[Lit].value == IntVal(2))
    assert(let2.afterLet.isInstanceOf[NoOp])
  }

  it must "return lit" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/lit_test.bnt")
    assert(exp.isInstanceOf[Lit])
    assert(exp.asInstanceOf[Lit].value == IntVal(5))
  }

  it should "parse let expression with multiple semicolons" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/semicolon_multi_parse_test.bnt")
    assert(exp.isInstanceOf[Let])
    val letExp = exp.asInstanceOf[Let]
    assert(letExp.ident == "a")
    assert(letExp.afterLet.isInstanceOf[Let])
    val letExp2 = letExp.afterLet.asInstanceOf[Let]
    assert(letExp2.ident == "dummy$2")
    assert(letExp2.afterLet.isInstanceOf[Let])
    val letExp3 = letExp2.afterLet.asInstanceOf[Let]
    assert(letExp3.ident == "dummy$1")
    assert(letExp3.afterLet.isInstanceOf[Let])
    val letExp4 = letExp3.afterLet.asInstanceOf[Let]
    assert(letExp4.ident == "dummy$0")
    assert(letExp4.afterLet.isInstanceOf[NoOp])
  }

  "Parser.dummyLet" must "return nested dummy lets" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/dummy_let_test.bnt")
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(!let.isLazy)
    assert(let.ident == "dummy$1")
    assert(let.letType == IntType())
    assert(let.expType == UnknownType())
    assert(let.expValue.isInstanceOf[Lit])
    assert(let.expValue.asInstanceOf[Lit].value == IntVal(2))
    assert(let.afterLet.isInstanceOf[Let])

    val let2 = let.afterLet.asInstanceOf[Let]
    assert(!let2.isLazy)
    assert(let2.ident == "dummy$0")
    assert(let2.letType == IntType())
    assert(let2.expType == UnknownType())
    assert(let2.expValue.isInstanceOf[Lit])
    assert(let2.expValue.asInstanceOf[Lit].value == IntVal(3))
    assert(let2.afterLet.isInstanceOf[NoOp])
  }

  "Parser.parseLet" must "return simplest let when given tokens" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/let_test.bnt")
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(!let.isLazy)
    assert(let.ident == "a")
    assert(let.letType == UnknownType())
    assert(let.expType == UnknownType())
    assert(let.expValue.isInstanceOf[Lit])
    assert(let.expValue.asInstanceOf[Lit].value == IntVal(0))
    assert(let.afterLet.isInstanceOf[NoOp])
  }

  it must "return let with annotations" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/let_test2.bnt")
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(let.isLazy)
    assert(let.ident == "a")
    assert(let.letType == IntType())
    assert(let.expType == UnknownType())
    assert(let.expValue.isInstanceOf[Lit])
    assert(let.expValue.asInstanceOf[Lit].value == IntVal(0))
    assert(let.afterLet.isInstanceOf[Ref])
    assert(let.afterLet.asInstanceOf[Ref].ident == "a")
  }

  "Parser.parseBranch" must "return branch exp" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/branch_test.bnt")
    assert(exp.isInstanceOf[Branch])
    val branch = exp.asInstanceOf[Branch]
    assert(branch.condition.isInstanceOf[Lit] &&
      branch.condition.asInstanceOf[Lit].value == BoolVal(true))
    assert(branch.ifBranch.isInstanceOf[Lit] &&
      branch.ifBranch.asInstanceOf[Lit].value == IntVal(5))
    assert(branch.elseBranch.isInstanceOf[Lit] &&
      branch.elseBranch.asInstanceOf[Lit].value == IntVal(4))
  }

  it must "return branch exp with nullval elseBranch if else dne" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/branch_test2.bnt")
    assert(exp.isInstanceOf[Branch])
    val branch = exp.asInstanceOf[Branch]
    print(branch)
    assert(branch.condition.isInstanceOf[Lit] &&
      branch.condition.asInstanceOf[Lit].value == BoolVal(true))
    assert(branch.ifBranch.isInstanceOf[Lit] &&
      branch.ifBranch.asInstanceOf[Lit].value == IntVal(5))
    assert(branch.elseBranch.isInstanceOf[Lit] &&
      branch.elseBranch.asInstanceOf[Lit].value == NullVal())
  }

  "Parser.parseCollectionValue" should "parse List collection" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/list_test.bnt")
    assert(exp.isInstanceOf[ListDef])
    val list = exp.asInstanceOf[ListDef]
    assert(list.values.length == 1)
    assert(list.values(0).isInstanceOf[Lit] &&
      list.values(0).asInstanceOf[Lit].value == IntVal(1))
  }

  it should "parse Array collection" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/array_test.bnt")
    assert(exp.isInstanceOf[ArrayDef])
    val array = exp.asInstanceOf[ArrayDef]
    assert(array.values.length == 1)
    assert(array.values(0).isInstanceOf[Lit] &&
      array.values(0).asInstanceOf[Lit].value == IntVal(1))
  }

  it should "parse Set collection" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/set_test.bnt")
    assert(exp.isInstanceOf[SetDef])
    val set = exp.asInstanceOf[SetDef]
    assert(set.values.length == 1)
    assert(set.values(0).isInstanceOf[Lit] &&
      set.values(0).asInstanceOf[Lit].value == IntVal(1))
  }

  it should "parse Tuple collection" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/tuple_test.bnt")
    assert(exp.isInstanceOf[TupleDef])
    val tuple = exp.asInstanceOf[TupleDef]
    assert(tuple.values.length == 2)
    assert(tuple.values(0).isInstanceOf[Lit] &&
      tuple.values(0).asInstanceOf[Lit].value == IntVal(1))
    assert(tuple.values(1).isInstanceOf[Lit] &&
      tuple.values(1).asInstanceOf[Lit].value.toString == "CharVal('a')")
  }

  it should "parse Dict collection" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/dict_test.bnt")
    assert(exp.isInstanceOf[DictDef])
    val dict = exp.asInstanceOf[DictDef]
    assert(dict.mapping.length == 1)
    assert(dict.mapping(0).key.isInstanceOf[Lit] &&
      dict.mapping(0).key.asInstanceOf[Lit].value == IntVal(1))
    assert(dict.mapping(0).value.isInstanceOf[Lit] &&
      dict.mapping(0).value.asInstanceOf[Lit].value.toString == "CharVal('a')")
  }

  it should "report unexpected" in {
    clear()
    tokenStream = Lexer.Lexer.scan("lazy")
    index = 0
    assert(parseCollectionValue.isInstanceOf[NoOp] && numErrors > 0)
  }

  "Parser.parseMatch" should "make a pattern match exp" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/match_test.bnt")
    assert(exp.isInstanceOf[Match])
    val matchExp = exp.asInstanceOf[Match]
    assert(matchExp.value.isInstanceOf[Ref] &&
      matchExp.value.asInstanceOf[Ref].ident == "x")
    assert(matchExp.cases.length == 2)
    assert(matchExp.cases(0).casePattern.isInstanceOf[ValueCase] &&
      matchExp.cases(0).casePattern.asInstanceOf[ValueCase].value == LitCase(IntVal(0)))
    assert(matchExp.cases(0).caseExp.isInstanceOf[Lit] &&
      matchExp.cases(0).caseExp.asInstanceOf[Lit].value == IntVal(1))

    assert(matchExp.cases(1).casePattern.isInstanceOf[ValueCase] &&
      matchExp.cases(1).casePattern.asInstanceOf[ValueCase].value == AnyCase())
    assert(matchExp.cases(1).caseExp.isInstanceOf[Lit] &&
      matchExp.cases(1).caseExp.asInstanceOf[Lit].value == IntVal(0))
  }

  it should "parse match with type case" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/match_type_case_test.bnt")
    assert(exp.isInstanceOf[Match])
    val matchExp = exp.asInstanceOf[Match]
    assert(matchExp.cases.length == 3)
    assert(matchExp.cases(0).casePattern.isInstanceOf[TypeCase])
    assert(matchExp.cases(0).casePattern.asInstanceOf[TypeCase].caseType.isInstanceOf[StringType])
    assert(matchExp.cases(1).casePattern.isInstanceOf[TypeCase])
    assert(matchExp.cases(1).casePattern.asInstanceOf[TypeCase].caseType.isInstanceOf[BoolType])
  }

  it should "parse match with constructor value case" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/match_constructor_case_test.bnt")
    assert(exp.isInstanceOf[Match])
    val matchExp = exp.asInstanceOf[Match]
    assert(matchExp.cases.length == 2)
    assert(matchExp.cases(0).casePattern.isInstanceOf[ValueCase])
    assert(matchExp.cases(0).casePattern.asInstanceOf[ValueCase].value.isInstanceOf[ConstructorCase])
    val constructorCase = matchExp.cases(0).casePattern.asInstanceOf[ValueCase].value.asInstanceOf[ConstructorCase]
    assert(constructorCase.ident == "Person")
    assert(constructorCase.values.length == 2)
    assert(constructorCase.values(0).isInstanceOf[ConstructorCase])
    assert(constructorCase.values(0).asInstanceOf[ConstructorCase].ident == "age")
    assert(constructorCase.values(0).asInstanceOf[ConstructorCase].values.isEmpty)
    assert(constructorCase.values(1).isInstanceOf[ConstructorCase])
    assert(constructorCase.values(1).asInstanceOf[ConstructorCase].ident == "name")
    assert(constructorCase.values(1).asInstanceOf[ConstructorCase].values.isEmpty)
  }

  it should "throw error in bad match literal value case" in {
    clear()
    tokenStream = Lexer.Lexer.scan("$")
    val valCase = parseValueCase
    assert(valCase.isInstanceOf[AnyCase] && numErrors > 0)
  }

  "Parser.warnAnyCase" should "warn against multiple wildcard cases" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/match_multi_wildcard_warning_test.bnt")
    assert(exp.isInstanceOf[Match])
    val matchExp = exp.asInstanceOf[Match]
    assert(matchExp.cases.length == 3)
    assert(warnings == 2)
  }

  it should "warn against match where wildcard is not the last pattern" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/match_no_wildcard_test.bnt")
    assert(exp.isInstanceOf[Match])
    val matchExp = exp.asInstanceOf[Match]
    assert(matchExp.cases.length == 2)
    assert(matchExp.cases(0).casePattern.isInstanceOf[ValueCase])
    assert(matchExp.cases(0).casePattern.asInstanceOf[ValueCase].value.isInstanceOf[AnyCase])
    assert(warnings == 1)
  }

  "Parser.parseAlias" should "parse an alias" in {
    clear()
    tokenStream = Lexer.Lexer.scan("alias IntType = int")
    val alias = parseAlias
    assert(alias.isInstanceOf[Alias])
    assert(alias.alias == "IntType")
    assert(alias.actualType.isInstanceOf[IntType])
  }

  "Parser.parseAdt" should "parse a data type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("type Option[T] derives Test { Some[T] | None }")
    val adt = parseAdt
    assert(adt.ident == "Option")
    assert(adt.generics.length == 1)
    assert(adt.generics.head.ident == "T")
    assert(adt.derivedFrom.ident == "Test")
    assert(adt.constructors.length == 2)
    assert(adt.constructors(0).members.length == 1)
    assert(adt.constructors(0).members(0).isInstanceOf[AdtType])
    assert(adt.constructors(0).members(0).asInstanceOf[AdtType].ident == "Some")
    assert(adt.constructors(1).members.length == 1)
    assert(adt.constructors(1).members(0).isInstanceOf[AdtType])
    assert(adt.constructors(1).members(0).asInstanceOf[AdtType].ident == "None")
  }

  "Parser.parseRecord" should "parse a record" in {
    clear()
    tokenStream = Lexer.Lexer.scan("record Person[T] extends Animal derives Test { age: int, name: string }")
    val record = parseRecord(true)
    assert(record.isSealed)
    assert(record.ident == "Person")
    assert(record.generics.length == 1)
    assert(record.generics.head.ident == "T")
    assert(record.superType.ident == "Animal")
    assert(record.derivedFrom.ident == "Test")
    assert(record.members.length == 2)
    assert(record.members(0).ident == "age")
    assert(record.members(0).memberType.isInstanceOf[IntType])
    assert(record.members(1).ident == "name")
    assert(record.members(1).memberType.isInstanceOf[StringType])
  }

  "Parser.parseTypeclass" should "parse typeclass" in {
    clear()
    tokenStream = Lexer.Lexer.scan("typeclass Ord[T] extends Eq { greater = (T, T) -> bool }")
    val typeclass = parseTypeclass(true)
    assert(typeclass.isSealed)
    assert(typeclass.ident == "Ord")
    assert(typeclass.genericTypes.length == 1)
    assert(typeclass.genericTypes.head.ident == "T")
    assert(typeclass.superclass.ident == "Eq")
    assert(typeclass.signatures.length == 1)
    assert(typeclass.signatures.head.name.ident == "greater")
    assert(typeclass.signatures.head.funcType.isInstanceOf[FuncType])
    assert(typeclass.signatures.head.funcType.asInstanceOf[FuncType].argTypes.length == 2)
    assert(typeclass.signatures.head.funcType.asInstanceOf[FuncType].returnType.isInstanceOf[BoolType])
  }

  "Parser.parseInstance" should "parse typeclass instance" in {
    clear()
    tokenStream = Lexer.Lexer.scan("instance Person : Eq { fn greater[T](a: T, b: T) -> a > b; }")
    val instance = parseInstance
    assert(instance.adt.ident == "Person")
    assert(instance.typeclassIdent.ident == "Eq")
    assert(instance.funcs.length == 1)
    assert(instance.funcs.head.ident == "greater")
    assert(instance.funcs.head.params.length == 2)
    assert(instance.funcs.head.generics.length == 1)
    assert(instance.funcs.head.generics.head.ident == "T")
  }

  "Parser.parseProg" should "parse 1 function definition" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/func_def_test.bnt")
    assert(exp.isInstanceOf[Prog])
    val funcDef = exp.asInstanceOf[Prog].funcs.head
    assert(funcDef.ident == "a")
    assert(funcDef.generics.isEmpty)
    assert(funcDef.params.isEmpty)
    assert(funcDef.returnType == IntType())
    assert(funcDef.body.isInstanceOf[Lit])
    assert(funcDef.body.asInstanceOf[Lit].value == IntVal(2))
  }

  it should "parse function with parametric type bounds" in {
    clear()
    tokenStream = Lexer.Lexer.scan("fn f[T :> R <: S]() -> bool = false;")
    val prog = parseProg
    assert(prog.funcs.length == 1)
    val func = prog.funcs.head
    assert(func.generics.length == 1)
    assert(func.generics.head.ident == "T")
    assert(func.generics.head.lowerBound == "R")
    assert(func.generics.head.upperBound == "S")
  }

  it should "parse 1 function with a default parameter" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/func_default_value_test.bnt")
    assert(exp.isInstanceOf[Prog])
    val progExp = exp.asInstanceOf[Prog]
    assert(progExp.funcs.length == 1)
    val funcDef = progExp.funcs.head
    assert(funcDef.ident == "a")
    assert(funcDef.generics.isEmpty)
    assert(funcDef.params.length == 1)
    assert(funcDef.params.head.ident == "x")
    assert(funcDef.params.head.paramType == IntType())
    assert(funcDef.params.head.default.isInstanceOf[Lit])
    assert(funcDef.params.head.default.asInstanceOf[Lit].value == IntVal(5))
    assert(funcDef.returnType == IntType())
    assert(funcDef.body.isInstanceOf[Prim])
    assert(funcDef.body.asInstanceOf[Prim].left.isInstanceOf[Ref])
    assert(funcDef.body.asInstanceOf[Prim].left.asInstanceOf[Ref].ident == "x")
    assert(funcDef.body.asInstanceOf[Prim].right.isInstanceOf[Lit])
    assert(funcDef.body.asInstanceOf[Prim].right.asInstanceOf[Lit].value == IntVal(2))
  }

  it should "parse 1 polymorphic function program exp" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/generic_func_test.bnt")
    assert(exp.isInstanceOf[Prog])
    val progExp = exp.asInstanceOf[Prog]
    assert(progExp.funcs.length == 1)
    val funcDef = progExp.funcs.head
    assert(funcDef.ident == "f")
    assert(funcDef.generics.length == 1)
    assert(funcDef.generics.head.ident == "T")
    assert(funcDef.params.length == 1)
    assert(funcDef.params.head.ident == "x")
    assert(funcDef.params.head.paramType == IntType())
    assert(funcDef.returnType == IntType())
    assert(funcDef.body.isInstanceOf[Ref])
    assert(funcDef.body.asInstanceOf[Ref].ident == "x")
    assert(progExp.afterProg.isInstanceOf[Let])
    val progBody = progExp.afterProg.asInstanceOf[Let]
    assert(!progBody.isLazy)
    assert(progBody.ident == "h")
    assert(progBody.letType.isInstanceOf[FuncType])
    val funcType = progBody.letType.asInstanceOf[FuncType]
    assert(funcType.argTypes.length == 1)
    assert(funcType.argTypes.head == IntType())
    assert(funcType.returnType == IntType())
    assert(progBody.expType == UnknownType())
    assert(progBody.expValue.isInstanceOf[Ref])
    assert(progBody.expValue.asInstanceOf[Ref].ident == "f")
    assert(progBody.afterLet.isInstanceOf[NoOp])
  }

  it should "parse a match expression inside a function definition" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/match_inside_func_def_test.bnt")
    assert(exp.isInstanceOf[Prog])
    val progExp = exp.asInstanceOf[Prog]
    assert(progExp.funcs.length == 1)
    val funcDef = progExp.funcs.head
    assert(funcDef.ident == "f")
    assert(funcDef.generics.isEmpty)
    assert(funcDef.params.isEmpty)
    assert(funcDef.returnType == IntType())
    assert(funcDef.body.isInstanceOf[Match])
    val matchExp = funcDef.body.asInstanceOf[Match]
    assert(matchExp.value.isInstanceOf[Ref])
    assert(matchExp.value.asInstanceOf[Ref].ident == "x")
    assert(matchExp.cases.length == 1)
    assert(matchExp.cases.head.isInstanceOf[Case])
    val caseExp = matchExp.cases.head
    assert(caseExp.casePattern.isInstanceOf[ValueCase])
    assert(caseExp.casePattern.asInstanceOf[ValueCase].value.isInstanceOf[AnyCase])
    assert(caseExp.caseExp.isInstanceOf[Lit])
    assert(caseExp.caseExp.asInstanceOf[Lit].value == IntVal(0))

    assert(progExp.afterProg.isInstanceOf[NoOp])
  }

  it should "parse closure made of function definitions" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/func_closure_test.bnt")
    assert(exp.isInstanceOf[Prog])
    val prog = exp.asInstanceOf[Prog]
    assert(prog.funcs.length == 1)
    val func = prog.funcs.head
    assert(func.ident == "a")
    assert(func.body.isInstanceOf[Prog])
    assert(func.body.asInstanceOf[Prog].funcs.length == 1)
    assert(func.body.asInstanceOf[Prog].funcs.head.ident == "b")
    assert(func.body.asInstanceOf[Prog].afterProg.isInstanceOf[Ref])
    assert(func.body.asInstanceOf[Prog].afterProg.asInstanceOf[Ref].ident == "b")
  }

  "Parser.parseLambda" should "parse lambda without parameters" in {
    clear()
    tokenStream = Lexer.Lexer.scan("|| -> bool = false")
    val lambda = parseLambda
    assert(lambda.funcs.length == 1)
    assert(lambda.funcs.head.ident == "anon$0_def")
    assert(lambda.funcs.head.generics.isEmpty)
    assert(lambda.funcs.head.params.isEmpty)
    assert(lambda.funcs.head.returnType.isInstanceOf[BoolType])
    assert(lambda.afterProg.isInstanceOf[Ref])
    assert(lambda.afterProg.asInstanceOf[Ref].ident == "anon$0_def")
  }

  it should "parse closure made of lambdas" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/lambda_closure_test.bnt")
    assert(exp.isInstanceOf[Prog])
    val prog = exp.asInstanceOf[Prog]
    assert(prog.funcs.length == 1)
    val lambda1 = prog.funcs.head
    assert(lambda1.ident == "anon$0_def")
    assert(lambda1.params.length == 1)
    assert(lambda1.params.head.ident == "x")
    assert(lambda1.body.isInstanceOf[Prog])
    assert(lambda1.body.asInstanceOf[Prog].funcs.length == 1)
    val lambda2 = lambda1.body.asInstanceOf[Prog].funcs.head
    assert(lambda2.params.length == 1)
    assert(lambda1.body.asInstanceOf[Prog].afterProg.isInstanceOf[Ref])
    assert(lambda1.body.asInstanceOf[Prog].afterProg.asInstanceOf[Ref].ident == "anon$1_def")
    assert(prog.afterProg.isInstanceOf[Ref])
    assert(prog.afterProg.asInstanceOf[Ref].ident == "anon$0_def")
  }

  it should "parse multiple function definitions with optional semicolons" in {
    clear()
    val exp = getExp("src/test/testPrograms/ParserPrograms/semicolon_parsing_test.bnt")
    assert(exp.isInstanceOf[Prog])
    val progExp = exp.asInstanceOf[Prog]
    assert(progExp.funcs.length == 4)
    assert(progExp.funcs.count(_.returnType == BoolType()) == 4)
    assert(progExp.afterProg.isInstanceOf[NoOp])
  }

  "Parser.parseUtight" should "parse unary NOT" in {
    clear()
    tokenStream = Lexer.Lexer.scan("!true")
    val exp = parseUtight(0)
    assert(exp.isInstanceOf[Prim])
    val prim = exp.asInstanceOf[Prim]
    assert(prim.left.isInstanceOf[Lit])
    assert(prim.left.asInstanceOf[Lit].value.isInstanceOf[BoolVal])
    // BoolVal.value is false
    assert(!prim.left.asInstanceOf[Lit].value.asInstanceOf[BoolVal].value)
    assert(prim.right.isInstanceOf[Lit])
    assert(prim.right.asInstanceOf[Lit].value.isInstanceOf[BoolVal])
    // BoolVal.value is true
    assert(prim.right.asInstanceOf[Lit].value.asInstanceOf[BoolVal].value)
    assert(prim.op == Delimiters.AND)
  }

  it should "parse unary MINUS" in {
    clear()
    tokenStream = Lexer.Lexer.scan("-1")
    val exp = parseUtight(0)
    assert(exp.isInstanceOf[Prim])
    val prim = exp.asInstanceOf[Prim]
    assert(prim.left.isInstanceOf[Lit])
    assert(prim.left.asInstanceOf[Lit].value.isInstanceOf[IntVal])
    assert(prim.left.asInstanceOf[Lit].value.asInstanceOf[IntVal].value == 0)
    assert(prim.right.isInstanceOf[Lit])
    assert(prim.right.asInstanceOf[Lit].value.isInstanceOf[IntVal])
    assert(prim.right.asInstanceOf[Lit].value.asInstanceOf[IntVal].value == 1)
    assert(prim.op == Delimiters.MINUS)
  }

  "Parser.parseApplication" should "parse function application" in {
    clear()
    tokenStream = Lexer.Lexer.scan("f[T](1)(2, 3)")
    val exp = parseApplication
    assert(exp.isInstanceOf[FuncApp])
    val outerApp = exp.asInstanceOf[FuncApp]
    assert(outerApp.ident.isInstanceOf[FuncApp])
    assert(outerApp.genericParameters.length == 1)
    assert(outerApp.genericParameters.head.isInstanceOf[AdtType])
    assert(outerApp.arguments.length == 2)
    assert(outerApp.arguments(0).isInstanceOf[Lit])
    assert(outerApp.arguments(0).asInstanceOf[Lit].value.isInstanceOf[IntVal])
    assert(outerApp.arguments(0).asInstanceOf[Lit].value.asInstanceOf[IntVal].value == 2)
    assert(outerApp.arguments(1).isInstanceOf[Lit])
    assert(outerApp.arguments(1).asInstanceOf[Lit].value.isInstanceOf[IntVal])
    assert(outerApp.arguments(1).asInstanceOf[Lit].value.asInstanceOf[IntVal].value == 3)
    val innerApp = outerApp.ident.asInstanceOf[FuncApp]
    assert(innerApp.ident.isInstanceOf[Ref])
    assert(innerApp.ident.asInstanceOf[Ref].ident == "f")
    assert(innerApp.genericParameters.length == 1)
    assert(innerApp.genericParameters.head.isInstanceOf[AdtType])
    assert(innerApp.arguments(0).isInstanceOf[Lit])
    assert(innerApp.arguments(0).asInstanceOf[Lit].value.isInstanceOf[IntVal])
    assert(innerApp.arguments(0).asInstanceOf[Lit].value.asInstanceOf[IntVal].value == 1)
  }

  "Parser.parseAtom" should "parse parentheses enclosed expression" in {
    clear()
    tokenStream = Lexer.Lexer.scan("(1 + 2)")
    val exp = parseAtom
    assert(exp.isInstanceOf[Prim])
    val prim = exp.asInstanceOf[Prim]
    assert(prim.left.isInstanceOf[Lit])
    assert(prim.left.asInstanceOf[Lit].value.isInstanceOf[IntVal])
    assert(prim.left.asInstanceOf[Lit].value.asInstanceOf[IntVal].value == 1)
    assert(prim.right.isInstanceOf[Lit])
    assert(prim.right.asInstanceOf[Lit].value.isInstanceOf[IntVal])
    assert(prim.right.asInstanceOf[Lit].value.asInstanceOf[IntVal].value == 2)
    assert(prim.op == Delimiters.PLUS)
  }

  it should "parse tuple access" in {
    clear()
    tokenStream = Lexer.Lexer.scan("t.0.1")
    val exp = parseAtom
    assert(exp.isInstanceOf[TupleAccess])
    val ta = exp.asInstanceOf[TupleAccess]
    assert(ta.ref.isInstanceOf[TupleAccess])
    assert(ta.accessIndex.value == 1)
    val taNested = ta.ref.asInstanceOf[TupleAccess]
    assert(taNested.ref.isInstanceOf[Ref])
    assert(taNested.ref.asInstanceOf[Ref].ident == "t")
    assert(taNested.accessIndex.value == 0)
  }

  "Parser.parseLit" should "parse null literal" in {
    clear()
    tokenStream = Lexer.Lexer.scan("null")
    val lit = parseLit
    assert(lit.isInstanceOf[Lit] &&
           lit.asInstanceOf[Lit].value.isInstanceOf[NullVal])
  }

  it should "parse string literal" in {
    clear()
    tokenStream = Lexer.Lexer.scan("\"hello\"")
    val lit = parseLit
    assert(lit.isInstanceOf[Lit] &&
           lit.asInstanceOf[Lit].value.isInstanceOf[StringVal] &&
           lit.asInstanceOf[Lit].value.asInstanceOf[StringVal].value == "\"hello\"")
  }

  it should "throw error on unexpected" in {
    clear()
    tokenStream = Lexer.Lexer.scan("lazy")
    val lit = parseLit
    assert(Parser.Parser.numErrors > 0)
  }

  "Parser.parseType" should "parse char type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("char")
    assert(parseType.isInstanceOf[CharType])
  }

  it should "parse string type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("string")
    assert(parseType.isInstanceOf[StringType])
  }

  it should "parse null type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("null")
    assert(parseType.isInstanceOf[NullType])
  }

  it should "parse list type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("List[int]")
    val listType = parseType
    assert(listType.isInstanceOf[ListType] &&
           listType.asInstanceOf[ListType].listType.isInstanceOf[IntType])
  }

  it should "parse array type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("Array[int]")
    val arrayType = parseType
    assert(arrayType.isInstanceOf[ArrayType] &&
           arrayType.asInstanceOf[ArrayType].arrayType.isInstanceOf[IntType])
  }

  it should "parse set type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("Set[int]")
    val setType = parseType
    assert(setType.isInstanceOf[SetType] &&
           setType.asInstanceOf[SetType].setType.isInstanceOf[IntType])
  }

  it should "parse tuple type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("Tuple[int, char]")
    val tupleType = parseType
    assert(tupleType.isInstanceOf[TupleType] &&
           tupleType.asInstanceOf[TupleType].tupleTypes.length == 2 &&
           tupleType.asInstanceOf[TupleType].tupleTypes(0).isInstanceOf[IntType] &&
           tupleType.asInstanceOf[TupleType].tupleTypes(1).isInstanceOf[CharType])
  }

  it should "parse dict type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("Dict[int, char]")
    val dictType = parseType
    assert(dictType.isInstanceOf[DictType] &&
           dictType.asInstanceOf[DictType].keyType.isInstanceOf[IntType] &&
           dictType.asInstanceOf[DictType].valueType.isInstanceOf[CharType])
  }

  it should "parse ident type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("Test")
    val identType = parseType
    assert(identType.isInstanceOf[AdtType] &&
           identType.asInstanceOf[AdtType].ident == "Test" &&
           identType.asInstanceOf[AdtType].generics.isEmpty)
  }

  it should "report bad type" in {
    clear()
    tokenStream = Lexer.Lexer.scan("lazy")
    val badType = parseType
    assert(badType.isInstanceOf[UnknownType] &&
           Parser.Parser.numErrors > 0)
  }
}