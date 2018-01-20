package lexer

import exceptions.LexException
import io.InputString
import java.math.{BigDecimal,BigInteger}
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import toplev.Pass

object Lexer extends Pass[InputString, LexemeSeq]("lex")
    with Parsers with RegexParsers {
  override val skipWhitespace = false

  lazy val comment: Parser[Lexeme] =
    """(?s)(\s|\(\*(.*?)\*\))+""".r ^^ { (_) => LexComment }

  lazy val nothing: Parser[Lexeme] = (
      "\\s+".r               ^^ { (_) => LexComment }
    | comment
  )

  lazy val nothingOrEmpty: Parser[Lexeme] = (
      nothing
    | ""                    ^^ { (_) => LexComment }
  )

  lazy val literal: Parser[Lexeme] = (
      "#\"" ~ escapedAsciiLiteral ~ "\"" ^^ {
      case (_ ~ char ~ _) => LexCharLiteral(char)
    }
    | "\"" ~ rep(escapedAsciiLiteral) ~ "\"" ^^ {
      case (_ ~ List() ~ _) => LexStringLiteral("")
      case (_ ~ chars ~ _) => LexStringLiteral(chars.mkString(""))
    }
    | zeroesInt ~ "." ~ zeroesNum ~ "(e|E)".r ~ zeroesInt ^^ {
      case (int ~ _ ~ dec ~ _ ~ exp) =>
        LexFloatLiteral(int._2, (dec._1, dec._2), exp._2)
    }
    | zeroesInt ~ "(e|E)".r ~ zeroesInt ^^ {
      case (int ~ _ ~ exp) =>
        LexFloatLiteral(int._2,
                        (0, LexIntLiteral(new BigInteger(new Array[Byte](1)))),
                        exp._2)
    }
    | zeroesInt ~ "." ~ zeroesNum ^^ {
      case (int ~ _ ~ dec) =>
        LexFloatLiteral(int._2, (dec._1, dec._2),
                        LexIntLiteral(new BigInteger(new Array[Byte](1))))
    }
    | num
    | "~" ~ nothingOrEmpty ~ num ^^ {
      case (_ ~ _ ~ LexIntLiteral(number)) => LexIntLiteral(number.negate())
    }
  )

  // For internal use:
  lazy val num: Parser[LexIntLiteral] = (
        "[1-9][0-9]*".r     ^^ { (int) => LexIntLiteral(new BigInteger(int)) }
      | "0"                 ^^ { (_) => LexIntLiteral(new BigInteger("0")) }
  )

  // This returns a tuple of the number of digits in the number and the
  // number itself (represented as a big int)
  lazy val zeroesInt: Parser[(Int, LexIntLiteral)] = (
         "~" ~ nothingOrEmpty ~ zeroesNum ^^ {
           case(_ ~ num) => (num._1, LexIntLiteral(num._2.value.negate()))
       }
       | zeroesNum
     )

  lazy val zeroesNum: Parser[(Int, LexIntLiteral)] = (
    "[0-9]+".r              ^^ {
        (int) => (int.length, LexIntLiteral(new BigInteger(int)))
    }
  )

  lazy val escapedAsciiLiteral: Parser[Char] = (
      opt(pad) ~ "\\\\" ~ opt(pad) ^^ { (_) => '\\' }
    | opt(pad) ~ "\\a" ~ opt(pad) ^^ { (_) => 7.toChar }
    | opt(pad) ~ "\\b" ~ opt(pad) ^^ { (_) => '\b' }
    | opt(pad) ~ "\\t" ~ opt(pad) ^^ { (_) => '\t' }
    | opt(pad) ~ "\\v" ~ opt(pad) ^^ { (_) => 11.toChar }
    | opt(pad) ~ "\\n" ~ opt(pad) ^^ { (_) => '\n' }
    | opt(pad) ~ "\\f" ~ opt(pad) ^^ { (_) => 12.toChar }
    | opt(pad) ~ "\\r" ~ opt(pad) ^^ { (_) => 13.toChar }
    | opt(pad) ~ "\\\\^[@-_]".r ~ opt(pad) ^^ {
      case (_ ~ string ~ _) => (string.charAt(2) - 64).toChar
    }
    | opt(pad) ~ "\\\\[0-9]{3}".r ~ opt(pad) ^^ {
      case (_ ~ string ~ _) =>
        Integer.parseInt(string.substring(1)).toChar
    }
    | opt(pad) ~ "\\\\u[0-9a-fA-F]{4}".r ~ opt(pad) ^^ {
      case (_ ~ string ~ _) =>
        Integer.parseInt("0x" + string.substring(2)).toChar
    }
    | opt(pad) ~ "\\\"" ~ opt(pad) ^^ { (_) => '"' }
    | opt(pad) ~ "[A-Za-z0-9_'`!@#$%^&*()\\-\\\\=+\\[\\]{};:<>.,/?~ ]".r
               ~ opt(pad) ^^ {
      case (_ ~ string ~ _) => string.charAt(0).toChar
    }
  )

  lazy val pad: Parser[String] = (
    """\\\s*\\""".r         ^^ { (_) => "" }
  )

  lazy val comma: Parser[Lexeme] =
    ","                     ^^ { (_) => LexComma }

  lazy val eqToken: Parser[Lexeme] =
    "="                     ^^ { (_) => LexEq }

  lazy val vBar: Parser[Lexeme] =
    "|"                     ^^ { (_) => LexVBar }

  lazy val colon: Parser[Lexeme] =
    ":"                     ^^ { (_) => LexColon }

  lazy val semiColon: Parser[Lexeme] =
    ";"                     ^^ { (_) => LexSemiColon }

  lazy val lexList: Parser[Lexeme] = (
      "[" ~ nothingOrEmpty ~ "]" ^^ { (_) => LexNil }
    | "["                   ^^ { (_) => LexLBrack }
    | "]"                   ^^ { (_) => LexRBrack }
  )

  lazy val lexParen: Parser[Lexeme] = (
      "(" ~ nothingOrEmpty ~ ")" ^^ { (_) => LexUnit }
    | "("                   ^^ { (_) => LexLParen }
    | ")"                   ^^ { (_) => LexRParen }
  )

  lazy val lexFn: Parser[Lexeme] = (
    "=>"                    ^^ { (_) => LexFnDecArrow }
  )

  lazy val lexLongIdent: Parser[Lexeme] = (
    lexIdent ~ opt("." ~ lexLongIdent) ^^ {
      case (LexIdentifier(name) ~ None) => LexIdentifier(name)
      case (other ~ None) => other
      case (LexIdentifier(name) ~ Some(_ ~ LexLongIdentifier(rest))) =>
        LexLongIdentifier(name :: rest)
      case (LexIdentifier(name) ~ Some(_ ~ LexIdentifier(rest))) =>
        LexLongIdentifier(List(name, rest))
      case (other ~ Some(_)) =>
        throw new LexException("Keyword as part of a long ident. ")
    }
  )

  lazy val lexIdent: Parser[Lexeme] = (
    "[A-Za-z_][A-Za-z0-9_']*".r ^^ {
      case "_" => LexUnderscore
      case "andalso" => LexAndAlso
      case "bool" => LexBoolType
      case "case" => LexCase
      case "char" => LexCharType
      case "datatype" => LexDatatype
      case "div" => LexIntDiv
      case "else" => LexElse
      case "end" => LexEnd
      case "false" => LexBoolFalse
      case "fn" => LexFn
      case "fun" => LexFun
      case "if" => LexIf
      case "in" => LexIn
      case "int" => LexIntType
      case "let" => LexLet
      case "list" => LexListType
      case "mod" => LexMod
      case "nil" => LexNil
      case "not" => LexNot
      case "of" => LexOf
      case "orelse" => LexOrElse
      case "print" => LexPrint
      case "real" => LexRealType
      case "string" => LexStringType
      case "then" => LexThen
      case "true" => LexBoolTrue
      case "unit" => LexUnitType
      case "val" => LexVal
      case name => LexIdentifier(name)
    }
  )

  // This needs to be done before the arithmetic and comparison
  // lexes.
  lazy val lexType: Parser[Lexeme] = (
      "->"                  ^^ { (_) => LexFunType }
    | "''" ~ variableName   ^^ { case (_ ~ name) => LexEqualityType(name) }
    | "'" ~ variableName    ^^ { case (_ ~ name) => LexUnconstrainedType(name) }
  )

  lazy val lexArithmetic: Parser[Lexeme] = (
      "+"                   ^^ { (_) => LexPlus }
    | "-"                   ^^ { (_) => LexMinus }
    | "/"                   ^^ { (_) => LexRealDiv }
    | "*"                   ^^ { (_) => LexTimes }
    | "~"                   ^^ { (_) => LexNeg }
  )

  lazy val stringOperations: Parser[Lexeme] =
    "^"                     ^^ { (_) => LexStringCat }


  lazy val comparison: Parser[Lexeme] = (
      "<="                  ^^ { (_) => LexLEQ }
    | ">="                  ^^ { (_) => LexGEQ }
    | "<>"                  ^^ { (_) => LexNeq }
    | ">"                   ^^ { (_) => LexGT }
    | "<"                   ^^ { (_) => LexLT }
  )

  lazy val listOperations: Parser[Lexeme] = (
      "::"                  ^^ { (_) => LexCons }
    | "@"                   ^^ { (_) => LexAppend }
  )

  lazy val variableName: Parser[String] = (
    "[0-9a-zA-Z'_]*".r      ^^ { (name) => name }
  )

  lazy val tokens: Parser[List[Lexeme]] = (
    phrase(rep(token))
  )

  lazy val token: Parser[Lexeme] = ((
        comment
      | comma
      // This needs to be done before arithmetic and comparison.
      | lexType
      // This also needs to be done before aithmetic and comparison.
      | lexFn
      | comparison
      | eqToken
      // Literal needs to be before identifiers and arithmetic.
      | literal
      | lexArithmetic
      | lexList
      | lexLongIdent
      | lexParen
      | listOperations
      // Colon needs to be after list operations.
      | colon
      | nothing
      | semiColon
      | stringOperations
      | vBar
    ))

  def run(input: InputString) = {
    val lexemes = parse(tokens, input.str)

    lexemes match {
      case Failure(msg, remaining) => {
        println("Lexer error")
        println(msg)
        System.exit(1)
        unreachable
      }
      case Error(msg, _) => {
        println("Error " + msg)
        System.exit(1)
        unreachable
      }
      case Success(tree, _) => {
        LexemeSeq(tree.filter(x => x != LexComment).toArray)
      }
    }
  }
}
