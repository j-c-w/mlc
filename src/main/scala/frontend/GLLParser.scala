package frontend

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import java.math.BigDecimal
import java.math.BigInteger
import sext._
import toplev.Pass

/* This file contains the parser description.  It has been
 * adapted from: https://people.mpi-sws.org/~rossberg/sml.html
 *
 * Refrences to headers correspond to that document
 * as do references to Omitted, Refactored etc.
 */

object GLLParser extends Pass[String, ASTProgram]("ast")
  with Parsers with RegexParsers {
  // This is used as a preliminary pass to strip comments
  // and excess whitespace.
  override def skipWhitespace = true
  override val whiteSpace = """(\s|\(\*([^*]|\*[^)])*\*\))+"""r

  // Constants

  lazy val con: Parser[ASTConst] = (
    // Omitted: word
    // Order swapped here as ints are a prefix of floats
      float
    | int
    | char
    | string
    | bool
  )

  lazy val int: Parser[ASTConstInt] = (
    num
    | "~" ~ num     ^^ { case (_ ~ ASTConstInt(int)) =>
                  ASTConstInt(int.negate()) }
    // Omitted: (~)0xhex (hexadecimal)
  )

  // Omitted: word

  lazy val float: Parser[ASTConstFloat] = (
    // Refactored: Replaced ~num with int
      int ~ "." ~ num ~ "e" ~ int ^^ { case (int ~ _ ~ dec ~ _ ~ exp) =>
            ASTConstFloat(int, dec, exp) }
    | int ~ "e" ~ int             ^^ { case (int ~ _ ~ exp) =>
            ASTConstFloat(int, ASTConstInt(new BigInteger(new Array[Byte](1))),
                          exp) }
    | int ~ "." ~ num             ^^ { case (int ~ _ ~ dec) =>
            ASTConstFloat(int, dec, ASTConstInt(new BigInteger(new
            Array[Byte](1)))) }
  )

  lazy val char: Parser[ASTConstChar] = (
    "#\"" ~ ascii ~ "\""      ^^ { case (_ ~ charList ~ _) => charList }
  )

  lazy val string: Parser[ASTConstString] = (
      // Refactored: replaced ascii* with asciiSeq
      "\"" ~ asciiSeq ~ "\""  ^^ { case (_ ~ string ~ _) =>
            string }
      // TODO -- extend the strings accepted
  )

  lazy val ascii: Parser[ASTConstChar] = (
    // Omitted: Escape sequences.
    // This regex matches all single characters
    // except ". Note that escaping is not supported.
    """[\\\]-~ !#-\[]""".r     ^^ { (char) => ASTConstChar(char.charAt(0)) }
  )

  lazy val asciiSeq: Parser[ASTConstString] = (
      ascii ~ asciiSeq         ^^ { case (ASTConstChar(char) ~
                                          ASTConstString(str)) =>
          ASTConstString(char + str) }
    | ""                       ^^ { (_) => ASTConstString("") }
  )

  lazy val bool: Parser[ASTConstBool] = (
      "true".r       ^^ { (_) => ASTConstTrue() }
    | "false".r      ^^ { (_) => ASTConstFalse() }
  )

  lazy val num: Parser[ASTConstInt] = (
    // Note, may not start with a 0
    """[1-9][0-9]*""".r        ^^ { (int) => ASTConstInt(new BigInteger(int)) }
    | "0"                      ^^ { (_) => ASTConstInt(new BigInteger("0")) }
  )

  // Identifiers
  
  // Refactored into 'id' and 'restrictedID' for use
  // in situations where not all the special characers can
  // be used
  lazy val id: Parser[ASTIdent] = (
    // Since these characters all have special meaning, we 
    // interpret them individually.
      restrictedID
    // Note that these are ordered so as to keep any
    // that prefix any others at the bottom
    | "::"              ^^ { (_) => ASTConsIdent() }
    | "[" ~ "]"         ^^ { case (_ ~ _) => ASTEmptyListIdent() }
    | "(" ~ ")"         ^^ { case (_ ~ _) => ASTUnitIdent() }
    | "<="              ^^ { (_) => ASTLEQIdent() }
    | ">="              ^^ { (_) => ASTGEQIdent() }
    | "<"               ^^ { (_) => ASTLTIdent() }
    | ">"               ^^ { (_) => ASTGTIdent() }
    | "="               ^^ { (_) => ASTEqIdent() }
    | "^"               ^^ { (_) => ASTStringCatIdent() }
    | "+"               ^^ { (_) => ASTPlusIdent() }
    | "-"               ^^ { (_) => ASTMinusIdent() }
    | "*"               ^^ { (_) => ASTTimesIdent() }
    | "/"               ^^ { (_) => ASTDivIdent() }
  )

  lazy val restrictedID: Parser[ASTIdent] = (
    // We add the restrictions on the front as
    // keywords are not valid identifiers.
    ("(?!fun|val|int|real|char|list|string|case|of|if|then|else|fn)" +
      "[A-Za-z][A-Za-z0-9_']*").r
                         ^^  { (str) => ASTIdentVar(str) }
  )

  lazy val longid: Parser[ASTLongIdent] = (
      id ~ "." ~ longid ^^ { case (ident1 ~ _ ~ ASTLongIdent(rest)) =>
            ASTLongIdent(ident1 :: rest) }
    | id                ^^ { (ident) => ASTLongIdent(List(ident)) }
  )

  // Inserted, UnOP
  lazy val unOp: Parser[ASTUnOp] = (
    "~"     ^^ { (_) => ASTUnOpNegate() }
    | "not" ^^ { (_) => ASTUnOpNot() }
  )

  // Restructure: rename var to tyvar as var is a keyword.
  lazy val tyvar: Parser[ASTTypeVar] = (
    // Restructure: Avoid ambiguity between unconstrained and
    // equality types. Use regex to avoid various space issues.
      "'[A-Za-z][A-Za-z0-9_']*".r          ^^ { (name) =>
                ASTUnconstrainedTypeVar(name) }
    | "''[A-Za-z0-9_']+".r                 ^^ { (name) =>
                ASTEqualityTypeVar(name) }
    // These are inserted as the grammar has omitted them.
    | "int"                                ^^ { (_) =>
                ASTIntType()
    }
    | "real"                               ^^ { (_) =>
                ASTRealType()
    }
    | "string"                             ^^ { (_) =>
                ASTStringType()
    }
    | "char"                               ^^ { (_) =>
                ASTCharType()
    }
    | "bool"                               ^^ { (_) =>
                ASTBoolType()
    }
    | "list"                               ^^ { (_) =>
                ASTListType()
    }
    | "[A-Za-z][A-Za-z0-9_']*".r           ^^ { (name) =>
                ASTDataTypeName(name)
    }
  )

  // Omitted: letter, digit. They cause issues with the lexing and
  // are better regex'd

  // Expressions
  // Refactored to avoid left recursion.
  lazy val exp: Parser[ASTExp] = (
      con ~ expTail                  ^^ { case (con ~ expTail)
            => expTail(ASTExpConst(con)) }
    // Inserted: UnOp ~ exp for dealing with unary operations
    | unOp ~ exp ~ expTail           ^^ { case (unop ~ exp ~ expTail)
            => expTail(ASTExpUnOpApply(unop, exp)) }
    | longid ~ expTail               ^^ { case (id ~ expTail)
            => expTail(ASTExpIdent(id)) }
    // Note that () and [] are treated as special values are so
    // are not considered as part of these expressions.
    | "(" ~ exp ~ ")" ~ expTail      ^^ { case (_ ~ exp ~ _ ~ expTail) =>
          expTail(exp) }
    | "[" ~ exp ~ "]" ~ expTail      ^^ { case (_ ~ exp ~ _ ~ expTail) => {
      exp match {
        // We do this as the list of exps are returned assuming
        // that they are to be put in a tuple. This extracts them and
        // puts them in the right type. Any other type is a singleton
        // list.
        case ASTExpTuple(list) => expTail(ASTExpList(list)) 
        case other => expTail(ASTExpList(List(other)))
      }
    } }
    // Refactored: (exp1 dots expn) replaced by the above
    // Omitted: raise exp
    // Omitted: exp handle match
    // Refactored: exp1; exp2; .. expN; replaced by expSeq
    // Note that none of these have EXP tails. This is intentional
    // as I do not believe that that is a meaningful grammar.
    // This may be subject to change.
    | "let" ~ dec ~ "in" ~ expSeq ~ "end" ^^ { case (_ ~ dec ~ _ ~ seq ~ _) =>
            ASTExpLetIn(dec, seq) }
    // Note that since 'else' is associated with all ifs, this avoids
    // the dangling else ambiguity.
    | "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp ^^ {
          case (_ ~ cond ~ _ ~ taken ~ _ ~ notTaken) =>
              ASTExpIfThenElse(cond, taken, notTaken)
    }
    // Rename  match to matchPat since match is a keyword in Scala
    | "case" ~ exp ~ "of" ~ matchPat ^^ { case (_ ~ exp ~ _ ~ matchPat) =>
              ASTExpCase(exp, matchPat) }
    | "fn" ~ matchPat        ^^ { case (_ ~ body) => ASTExpFn(body) }
  )

  lazy val expTail: Parser[ASTExp => ASTExp] = (
    id ~ exp         ^^ { case (id ~ op2) =>
        ((op1: ASTExp) => ASTExpInfixApp(id, op1, op2)) }
    | exp              ^^ { case (app) =>
        ((fun: ASTExp) => ASTExpFunApp(fun, app))
    }
    | ":" ~ typ        ^^ { case (_ ~ typ) =>
          ((exp: ASTExp) => ASTExpTyped(exp, typ)) }
    | "orelse" ~ exp   ^^ { case (_ ~ e2) =>
          ((e1: ASTExp) => ASTExpOr(e1, e2)) }
    | "andalso" ~ exp  ^^ { case (_ ~ e2) =>
          ((e1: ASTExp) => ASTExpAnd(e1, e2)) }
    | "," ~ exp        ^^ { case (_ ~ tail) =>
          ((first: ASTExp) => tail match {
            case ASTExpTuple(astExp) => ASTExpTuple(first :: astExp)
            case notList => ASTExpTuple(List(first, notList))
          })}
    | ";" ~ exp        ^^ { case (_ ~ tail) =>
          ((first: ASTExp) => tail match {
            case ASTExpSeq(last) => ASTExpSeq(first :: last)
            case otherExp => ASTExpSeq(List(first, otherExp))
          })}
    | ""               ^^ { (_) => (x: ASTExp) => x }
  )

  // Inserted. To deal with the restriction that (epx1, ..., expN)
  // is a tuple iff N != 1 (but can be 0)
  lazy val anyLengthExpList: Parser[List[ASTExp]] = (
      ""                               ^^ { _ => List[ASTExp]() }
    | exp ~ "," ~ anyLengthExpList     ^^ { case (e1 ~ _ ~ rest) =>
        e1 :: rest }
  )

  // This accepts seqs of one or more.
  lazy val expSeq: Parser[List[ASTExp]] = (
      exp ~ ";" ~ expSeq ^^ { case (exp ~ _ ~ expSeq) => exp :: expSeq }
    | exp                ^^ { (e) => List(e) }
  )

  // Omitted: exprow

  // Renamed to matchPat since match is a keyword in scala
  lazy val matchPat: Parser[List[ASTExpMatchRow]] = (
    // Note that this is not allowed to be empty
      pat ~ "=>" ~ exp ~ "|" ~ matchPat     ^^ {
          case (pat ~ _ ~ exp ~ _ ~ rest) =>
                ASTExpMatchRow(pat, exp) :: rest }
    | pat ~ "=>" ~ exp                      ^^ { case (pat ~ _ ~ exp) =>
                List(ASTExpMatchRow(pat, exp)) }
  )

  // Patterns
  
  // Restructured to avoid left recursion.
  lazy val pat: Parser[ASTPat] = (
      con ~ patTail                     ^^ { case (con ~ patTail)
            => patTail._1(ASTPatConst(con, patTail._2)) }
    | "_" ~ patTail                     ^^ { case (_ ~ patTail)
            => patTail._1(ASTPatWildcard(patTail._2)) }
    // Restricted here as special characters may not appear
    // in pat lists.
    | restrictedID  ~ patTail           ^^ { case (id ~ patTail)
            => patTail._1(ASTPatVariable(id, patTail._2)) }
    // Omitted: (op) longid (pat); construction
    // Omitted: (pat) id (pat)
    // Special case: pat id pat -> pat :: pat.
    // Restructured: (pat1, ... patN) replaced with (patlist)
    // Restructured: (pat) replaced with (patList)
    | "(" ~ patList ~ ")" ~ patTail     ^^ {
          case (_ ~ ASTPatSeq(patList, _) ~ _ ~ patTail) =>
                   patTail._1(ASTPatSeq(patList, patTail._2)) }
    // Note that there is a distinction between ASTPatSeq and
    // ASTListPat. The former is a pattern list and the later
    // is a list of patterns.
    | "[" ~ patList ~ "]" ~ patTail     ^^ {
          case (_ ~ ASTPatSeq(patSeq, _) ~ _ ~ patTail) => 
                   patTail._1(ASTListPat(patSeq, patTail._2)) }
    // Omitted: (op) id (:typ) as pat; layed
  )

  lazy val patTail: Parser[((ASTPat => ASTPat), List[ASTType])] = (
      "::" ~ pat                        ^^ { case (_ ~ pat) =>
        ((prePat: ASTPat) => ASTPatCons(prePat, pat), List[ASTType]()) }
    | ":" ~ typ ~ typList               ^^ { case (_ ~ typ ~ typList) =>
        (((x: ASTPat) => x), (typ :: typList)) }
    | ""                                ^^ { _ =>
        (((x: ASTPat) => x), List[ASTType]()) }
  )

  lazy val typList: Parser[List[ASTType]] = (
      ":" ~ typ ~ typList              ^^ { case (_ ~ typ ~ typList) =>
              typ :: typList }
    | ""  ^^ { (_) => List[ASTType]() }
  )

  // Note that the second argument of ASTPatSeq is a list of 
  // types that is assigned to this sequence (as a WHOLE)
  lazy val patList: Parser[ASTPatSeq] = (
      pat ~ "," ~ patList            ^^ { case (pat ~ _ ~ ASTPatSeq(list, Nil))
            => ASTPatSeq(pat :: list, Nil) }
    | pat                            ^^ { (pat) => ASTPatSeq(List(pat), Nil) }
    | ""                             ^^ { (_) => ASTPatSeq(Nil, Nil) }
  )
  
  // Omitted: patrow

  // Types
  
  lazy val typ: Parser[ASTType] = (
    // Restructure: Rename var to tyvar as var is a keyword.
    // Restructure: use typTail to avoid ambiguity
    tyvar ~ typTail     ^^ { case (tyvar ~ tail) => tail(tyvar)}
    // Omitted: (typ)(,) longid; constructor
    | "(" ~ typ ~ ")" ~ typTail  ^^ { case (_ ~ typ ~ _ ~ typTail) =>
            typTail(typ) }
    // Omitted: { (typrow) }; record
  )

  lazy val typTail: Parser[(ASTType => ASTType)] = (
    "->" ~ typ ~ typTail      ^^ { case (_ ~ typ1 ~ typTail) =>
       ((typ: ASTType) => typTail(ASTTypeFunction(typ, typ1))) }
    |  "*" ~ typ ~ typTail    ^^ { case (_ ~ typ1 ~ typTail) =>
       ((typ: ASTType) => typ match {
         case (ASTTypeTuple(tail)) => typTail(ASTTypeTuple(typ :: tail))
         case (otherTyp) => typTail(ASTTypeTuple(List(typ, typ1)))
       })
    }
    |  ""                     ^^ { _ => ((x: ASTType) => x) }
  )

  // Omitted: typrow

  //  Declarations

  lazy val dec: Parser[List[ASTDeclaration]] = (
    // Omitted: val (val)(,) valbind
      "val" ~ valbind                ^^ { case (_ ~ valbind) => List(valbind) }
    // Omitted: val (val)(,) funbind
    | "fun" ~ funbind                ^^ { case (_ ~ funbind) => List(funbind) }
    // Omitted: type typebind
    // Omitted: (with typebind)
    | "datatype" ~ datbind           ^^ { case (_ ~ datbind) => List(datbind) }
    // Omitted: abstype datbind (withtype typbind) with dec end
    // Omitted: exnbind
    // Omitted: structure
    // Omitted: local dec1 in dec2 end
    // Omitted: open longid1 ... longidN
    // Omitted nonfix id1 ... idN
    // Omitted infix id1 ... idN
    // Omitted infixr id1 ... idN
  )

  lazy val valbind: Parser[ASTDeclaration] = (
      // Refactor: Replace pat with valIDs to keep some
      // simplicity. Cost is some valid expressions.
      // Restricted as special characters may not appear
      // Refactor: 
    valIDs ~ "=" ~ exp  ^^ {
        case (id ~ _ ~ exp) => ASTValBind(id, exp) }
      // Omitted pat = exp and valbind
      // Omitted rec valbind
  )

  lazy val valIDs: Parser[List[ASTIdent]] = (
      "(" ~> restrictedIDList <~ ")"
    | restrictedID                    ^^ { (id) => List(id) }
  )

  lazy val restrictedIDList: Parser[List[ASTIdent]] = (
      restrictedID ~ "," ~ restrictedIDList ^^ { case (id ~ _ ~ idList) =>
              id :: idList
      }
      | restrictedID                        ^^ { case (id) =>
              List[ASTIdent](id)
      }
  )

  lazy val funbind: Parser[ASTDeclaration] = (
    funmatch
    //  Omitted funmatch and funbind
  )

  lazy val funmatch: Parser[ASTFun] = (
    // Omitted: (op) prefix
    // Restructured: replace pat1 ... patn with patN parser.
      restrictedID ~ patN ~ "=" ~ exp ~ "|" ~ funmatch             ^^ {
          case (id ~ pattern ~ _ ~ exp ~ _ ~ ASTFunBind(patList))
            => ASTFunBind((id, pattern, None, exp) :: patList) }
    | restrictedID ~ patN ~ ":" ~ typ ~ "=" ~ exp ~ "|" ~ funmatch ^^ {
          case (id ~ pattern ~ _ ~ typ ~ _ ~ exp ~ _ ~ ASTFunBind(patList))
            => ASTFunBind((id, pattern, Some(typ), exp) :: patList) }
    | restrictedID ~ patN ~ "=" ~ exp                              ^^ {
          case (id ~ pattern ~ _ ~ exp)
            => ASTFunBind(List((id, pattern, None, exp))) }
    | restrictedID ~ patN ~ ":" ~ typ ~ "=" ~ exp                  ^^ {
          case (id ~ pattern ~ _ ~ typ ~ _ ~ exp)
            => ASTFunBind(List((id, pattern, Some(typ), exp))) }
    // Omitted: infix patterns:
    //   pat1 id pat2 (: typ) = exp (funmatch)
    //   (pat1 id pat2) pat`1 ... pat`N (: typ) = exp (| funmatch)
  )

  lazy val patN: Parser[List[ASTPat]] = (
      pat ~ patN      ^^ { case (pat ~ patList) => pat :: patList }
    | pat             ^^ { (pat) => List(pat) }
  )

  lazy val datbind: Parser[ASTDeclaration] = (
    // Omitted: var (,) id = conbind (and datbind)
    // Omitted: var (,) prefix
    restrictedID ~ "=" ~ conbind       ^^ { case (id ~ _ ~ conbind)
          => ASTDataType(id, conbind) }
  )

  lazy val conbind: Parser[List[ASTDataConstructor]] = (
      restrictedID ~ "of" ~ typ ~ "|" ~ conbind ^^ {
        case (id ~ _ ~ typ ~ _ ~ rest)
            => ASTDataConstructorDefinitionWithType(id, typ) :: rest }
    | restrictedID ~ "of" ~ typ                 ^^ { case (id ~ _ ~ typ)
            => List(ASTDataConstructorDefinitionWithType(id, typ)) }
    | restrictedID ~ "|" ~ conbind              ^^ { case (id ~ _ ~ rest)
            => ASTDataConstructorDefinition(id) :: rest }
    | restrictedID                              ^^ { x
            => List(ASTDataConstructorDefinition(x)) }
  )

  // Programs

  lazy val prog: Parser[List[ASTDeclaration]] = (
    dec ~ prog  ^^ { case (dec ~ prog) => dec ::: prog }
    | "\0"   ^^ { (_) => List[ASTDeclaration]() }
  )

  def treeToString(input: ASTProgram) =
    input.prettyPrint + "\n\n__ Formatted Version __\n\n" +
    input.toString

  private def charSequenceReaderToString(build: String, input: Reader[Char])
          : String = input.first match {
      case CharSequenceReader.EofCh => build
      case character => charSequenceReaderToString(build + character,
                                                   input.rest)
    }

  protected def run(code: String) = {
    val ast = parse(prog, code + "\0")

    ast match {
      case Failure(msg, remaining) => {
          println("Syntax Error")
          println(msg)
          println("Rest of input is: ")
          println(charSequenceReaderToString("", remaining))
          System.exit(1)
          null
        }
      case Error(msg, _) => {
        println("Error" + msg)
        System.exit(1)
        null
      }
      case Success(tree, _) => {
        println("Success")
        new ASTProgram(tree)
      }
    }
  }
}
