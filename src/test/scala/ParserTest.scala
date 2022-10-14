import org.scalatest.flatspec.AnyFlatSpec
import Lexer.SyntaxDefinitions.Delimiters._
import Lexer.SyntaxDefinitions.Keywords._
import Lexer.{Delimiter, EOF, FilePosition, Ident, Keyword, Terminator, Token}
import Logger.Logger.lineList
import Parser.{AnyCase, ArrayDef, BoolVal, Branch, Case, CharVal, DictDef, FunDef, IntVal, Let, ListDef, Lit, LitCase, Match, NoOp, NullVal, Prim, Prog, Ref, SetDef, TupleDef, ValueCase}
import Parser.Parser._
import TypeChecker.{BoolType, FuncType, IntType, UnknownType}

import scala.collection.mutable.ArrayBuffer

class ParserTest extends AnyFlatSpec {
  def clear(): Unit = {
    index = 0
    tokenStream = ArrayBuffer[Token]()
    errorOccurred = false
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
    assert(!matchRequired(LAZY) && index == 1 && errorOccurred)
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
    assert(!matchOptional(LAZY) && index == 0 && !errorOccurred)
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
    assert(matchIdent != "name" && index == 0)
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

  "Parser.dummyLet" must "return nested dummy lets" in {
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

  "Parser.parseExp" must "return simplest let when given tokens" in {
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
    val exp = getExp("src/test/testPrograms/ParserPrograms/lit_test.bnt")
    assert(exp.isInstanceOf[Lit])
    assert(exp.asInstanceOf[Lit].value == IntVal(5))
  }

  "Parser.parseLet" must "return simplest let when given tokens" in {
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
    val exp = getExp("src/test/testPrograms/ParserPrograms/list_test.bnt")
    assert(exp.isInstanceOf[ListDef])
    val list = exp.asInstanceOf[ListDef]
    assert(list.values.length == 1)
    assert(list.values(0).isInstanceOf[Lit] &&
      list.values(0).asInstanceOf[Lit].value == IntVal(1))
  }

  it should "parse Array collection" in {
    val exp = getExp("src/test/testPrograms/ParserPrograms/array_test.bnt")
    assert(exp.isInstanceOf[ArrayDef])
    val array = exp.asInstanceOf[ArrayDef]
    assert(array.values.length == 1)
    assert(array.values(0).isInstanceOf[Lit] &&
      array.values(0).asInstanceOf[Lit].value == IntVal(1))
  }

  it should "parse Set collection" in {
    val exp = getExp("src/test/testPrograms/ParserPrograms/set_test.bnt")
    assert(exp.isInstanceOf[SetDef])
    val set = exp.asInstanceOf[SetDef]
    assert(set.values.length == 1)
    assert(set.values(0).isInstanceOf[Lit] &&
      set.values(0).asInstanceOf[Lit].value == IntVal(1))
  }

  it should "parse Tuple collection" in {
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
    val exp = getExp("src/test/testPrograms/ParserPrograms/dict_test.bnt")
    assert(exp.isInstanceOf[DictDef])
    val dict = exp.asInstanceOf[DictDef]
    assert(dict.keys.length == 1)
    assert(dict.keys(0).isInstanceOf[Lit] &&
      dict.keys(0).asInstanceOf[Lit].value == IntVal(1))
    assert(dict.values(0).isInstanceOf[Lit] &&
      dict.values(0).asInstanceOf[Lit].value.toString == "CharVal('a')")
  }

  "Parser.parseMatch" should "make a pattern match exp" in {
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

  "Parser.parseFunctionDefinition" should "parse 1 function definition" in {
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

  "Parser.parseSimpleGenericFunction" should "parse a 1 polymorphic function program exp" in {
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
    assert(progExp.body.isInstanceOf[Let])
    val progBody = progExp.body.asInstanceOf[Let]
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

  "Parser.parseMatchInsideFuncDef" should "parse a match expression inside a function definition" in {
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

    assert(progExp.body.isInstanceOf[NoOp])
  }

  "Parser.parseFuncClosure" should "parse closure made of function definitions" in {
    val exp = getExp("src/test/testPrograms/ParserPrograms/func_closure_test.bnt")
    assert(exp.isInstanceOf[Prog])
    val prog = exp.asInstanceOf[Prog]
    assert(prog.funcs.length == 1)
    val func = prog.funcs.head
    assert(func.ident == "a")
    assert(func.body.isInstanceOf[Prog])
    assert(func.body.asInstanceOf[Prog].funcs.length == 1)
    assert(func.body.asInstanceOf[Prog].funcs.head.ident == "b")
    assert(func.body.asInstanceOf[Prog].body.isInstanceOf[Ref])
    assert(func.body.asInstanceOf[Prog].body.asInstanceOf[Ref].ident == "b")
  }

  "Parser.parseLambdaClosure" should "parse closure made of lambdas" in {
    val exp = getExp("src/test/testPrograms/ParserPrograms/lambda_closure_test.bnt")
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(let.expValue.isInstanceOf[Let])
    val lambda1 = let.expValue.asInstanceOf[Let]
    assert(lambda1.ident == "anon$0")
    assert(lambda1.expValue.isInstanceOf[FunDef])
    assert(lambda1.expValue.asInstanceOf[FunDef].ident == "anon$0_def")
    assert(lambda1.expValue.asInstanceOf[FunDef].body.isInstanceOf[Let])
    val lambda2 = lambda1.expValue.asInstanceOf[FunDef].body.asInstanceOf[Let]
    assert(lambda2.ident == "anon$1")
    assert(lambda2.expValue.isInstanceOf[FunDef])
    assert(lambda2.expValue.asInstanceOf[FunDef].ident == "anon$1_def")
    assert(lambda2.expValue.asInstanceOf[FunDef].body.isInstanceOf[Prim])
  }

  "Parser.parseSemicolon" should "parse multiple function definitions with optional semicolons" in {
    val exp = getExp("src/test/testPrograms/ParserPrograms/semicolon_parsing_test.bnt")
    assert(exp.isInstanceOf[Prog])
    val progExp = exp.asInstanceOf[Prog]
    assert(progExp.funcs.length == 4)
    assert(progExp.funcs.count(_.returnType == BoolType()) == 4)
    assert(progExp.body.isInstanceOf[NoOp])
  }

  "Parser.parseSemicolonMulti" should "parse let expression with multiple semicolons" in {
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

  "Parser.dummy" must "return dummy name" in {
    clear()
    assert(dummy == "dummy$0")
  }

  "Parser.anon" must "return anon name" in {
    clear()
    assert(anon == "anon$0")
  }
}