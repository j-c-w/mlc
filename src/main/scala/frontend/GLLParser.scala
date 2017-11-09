package frontend

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import java.math.BigDecimal
import java.math.BigInteger
import toplev.Pass

/* This file contains the parser description.  It has been
 * adapted from: https://people.mpi-sws.org/~rossberg/sml.html
 *
 * Refrences to headers correspond to that document
 * as do references to Omitted, Refactored etc.
 */

/* Known issues:
 *    We could be more accepting of ';'. The current parser
 *    rejects them where they are harmless (but not useful).
 *    It just means that other implementations compile things that
 *    CMLC does not compile.
 *
 *    Nested comments are currently not supported. They probably should
 *    be.
 *
 *    Type annotations on patterns are currently not supported. They probably
 *    should be.
 *
 *    Type annotations on functions don't work as expected. This should be
 *    resolved.
 *
 *    There are issues with the prceedence of 'andalso' and 'orelse'. They
 *    should be lower precedence than the other infix operators, but
 *    are treated equally.
 */

object GLLParser extends Pass[String, ASTProgram]("ast")
  with Parsers with RegexParsers {
  // This is used as a preliminary pass to strip comments
  // and excess whitespace.
  override def skipWhitespace = true
  override val whiteSpace = """(?s)(\s|\(\*(.*?)\*\))+"""r

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
    // Note that these are ordered so as to keep any
    // that prefix any others at the bottom
      "(" ~ ")"         ^^ { case (_ ~ _) => ASTUnitIdent() }
    | restrictedIDAllowList
  )

  lazy val restrictedIDAllowList: Parser[ASTIdent] = (
      "[" ~ "]"         ^^ { case (_ ~ _) => ASTEmptyListIdent() }
    | "nil"             ^^ { (_) => ASTEmptyListIdent() }
    | restrictedID
  )

  lazy val restrictedID: Parser[ASTIdent] = (
    // We add the restrictions on the front as
    // keywords are not valid identifiers.
    ("(?!fun[^A-Za-z0-9_']|val[^A-Za-z0-9_']|int[^A-Za-z0-9_']|" +
      "real[^A-Za-z0-9_']|char[^A-Za-z0-9_']|list[^A-Za-z0-9_']|" +
      "string[^A-Za-z0-9_']|case[^A-Za-z0-9_']|of[^A-Za-z0-9_']|" +
      "if[^A-Za-z0-9_']|then[^A-Za-z0-9_']|else[^A-Za-z0-9_']|" +
      "fn[^A-Za-z0-9_']|nil[^A-Za-z0-9_']|let[^A-Za-z0-9_']|" +
      "in[^A-Za-z0-9_']|end[^A-Za-z0-9_']|orelse[^A-Aa-z0-9_']|" +
      "andalso[^A-Za-z0-9_']|mod[^A-Za-z0-9_']|div[^A-Za-z0-9_']|" +
      "print[^A-Za-z0-9_'])([A-Za-z][A-Za-z0-9_']*)").r
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
    "'[A-Za-z][A-Za-z0-9_']*".r ~ tyvarTail  ^^ { case (name ~ tail) =>
                tail(ASTUnconstrainedTypeVar(name.substring(1))) }
    | "''[A-Za-z0-9_']+".r ~ tyvarTail       ^^ { case (name ~ tail) =>
                tail(ASTEqualityTypeVar(name.substring(2))) }
    // These are inserted as the grammar has omitted them.
    | "int" ~ tyvarTail                      ^^ { case (_ ~ tail) =>
                tail(ASTIntType())
    }
    | "real" ~ tyvarTail                     ^^ { case (_ ~ tail) =>
                tail(ASTRealType())
    }
    | "string" ~ tyvarTail                   ^^ { case (_ ~ tail) =>
                tail(ASTStringType())
    }
    | "char" ~ tyvarTail                     ^^ { case (_ ~ tail) =>
                tail(ASTCharType())
    }
    | "bool" ~ tyvarTail                     ^^ { case (_ ~ tail) =>
                tail(ASTBoolType())
    }
    // Refactored: Lists are the tail here because they are
    // left recursive
    | "[A-Za-z][A-Za-z0-9_']*".r ~ tyvarTail ^^ { case (name ~ tail) =>
                tail(ASTDataTypeName(name))
    }
  )

  lazy val tyvarTail: Parser[ASTTypeVar => ASTTypeVar] = (
    "list" ~ tyvarTail                     ^^ { case (_ ~ tail) =>
      ((ty: ASTTypeVar) => tail(ASTListType(ty)))
    }
    | ""                                   ^^ { (_) => ((x: ASTTypeVar) => x) }
  )

  // Omitted: letter, digit. They cause issues with the lexing and
  // are better regex'd

  // Expressions
  // Refactored to avoid left recursion.
  lazy val exp: Parser[ASTExp] = (
      infixApp ~ expTail             ^^ { case (infix ~ expTail)
            => expTail(infix) }
    // Inserted: UnOp ~ exp for dealing with unary operations
    | unOp ~ exp ~ expTail           ^^ { case (unop ~ exp ~ expTail)
            => expTail(ASTExpUnOpApply(unop, exp)) }
    // Refactored: (exp1 dots expn) replaced by the above
    // Omitted: raise exp
    // Omitted: exp handle match
    // Refactored: exp1; exp2; .. expN; replaced by expSeq
    // Note that none of these have EXP tails. This is intentional
    // as I do not believe that that is a meaningful grammar.
    // This may be subject to change.
    // Note that since 'else' is associated with all ifs, this avoids
    // the dangling else ambiguity.
    | "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp ^^ {
          case (_ ~ cond ~ _ ~ taken ~ _ ~ notTaken)
            => ASTExpIfThenElse(cond, taken, notTaken)
    }
    // Rename  match to matchPat since match is a keyword in Scala
    | "case" ~ exp ~ "of" ~ matchPat ^^ { case (_ ~ exp ~ _ ~ matchPat)
            => ASTExpCase(exp, matchPat) }
    | "fn" ~ matchPat        ^^ { case (_ ~ body) => ASTExpFn(body) }
  )

  lazy val infixApp: Parser[ASTExp] = (
    infixTop
  )
  // Inserted: These are infix operator precedences, in
  // increasing order. Infix 0 is the lowest procedence.
  // 'before' is not part of my subset.
  // lazy val infix0: Parser
  // There are no values at 1, 2
  // lazy val infix1: Parser
  // lazy val infix2: Parser
  // 'o' and := are not part of my subset
  // lazy val infix3: Parser

  // Artificiall inserted to get these to bind the right way.
  lazy val infixTop: Parser[ASTExp] = (
    infix4 ~ infixTopTail  ^^ {
        case (exp ~ tail) =>
          tail(exp)
    }
  )

  lazy val infixTopTail: Parser[(ASTExp => ASTExp)] = (
      "orelse" ~ infixTop    ^^ { case (_ ~ tail)
          => ((x: ASTExp) => ASTExpInfixApp(ASTOrIdent(), x, tail)) }
    | "andalso" ~ infixTop   ^^ { case (_ ~ tail)
          => ((x: ASTExp) => ASTExpInfixApp(ASTAndIdent(), x, tail)) }
    | expTail                ^^ { (tail) => ((x: ASTExp) => tail(x)) }
  )

  lazy val infix4: Parser[ASTExp] = (
    infix5 ~ infix4Tail  ^^ {
        case (exp ~ tail) =>
          tail(exp)
    }
  )

  lazy val infix4Tail: Parser[(ASTExp => ASTExp)] = (
      "<=" ~ infix4        ^^ { case(_ ~ tail)
          => ((x: ASTExp) =>
                ASTExpInfixApp.leftAssociate(ASTLEQIdent(), x, tail)) }
    | ">=" ~ infix4        ^^ { case(_ ~ tail)
          => ((x: ASTExp) =>
              ASTExpInfixApp.leftAssociate(ASTGEQIdent(), x, tail)) }
    | "<"  ~ infix4        ^^ { case(_ ~ tail)
          => ((x: ASTExp) =>
              ASTExpInfixApp.leftAssociate(ASTLTIdent(), x, tail)) }
    | ">"  ~ infix4        ^^ { case(_ ~ tail)
          => ((x: ASTExp) =>
              ASTExpInfixApp.leftAssociate(ASTGTIdent(), x, tail)) }
    | "="  ~ infix4        ^^ { case(_ ~ tail)
          => ((x: ASTExp) =>
              ASTExpInfixApp.leftAssociate(ASTEqIdent(), x, tail)) }
    | ""                   ^^ { (_) => ((x: ASTExp) => x) }
  )

  lazy val infix5: Parser[ASTExp] = (
    infix6 ~ infix5Tail    ^^ { case (exp ~ tail) => tail(exp) }
  )

  lazy val infix5Tail: Parser[(ASTExp => ASTExp)] = (
      "::" ~ infix5        ^^ { case(_ ~ tail)
          => ((x: ASTExp) => ASTExpInfixApp(ASTConsIdent(), x, tail)) }
    | "@"  ~ infix5        ^^ { case(_ ~ tail)
          => ((x: ASTExp) => ASTExpInfixApp(ASTAppendIdent(), x, tail)) }
    | ""                   ^^ { (_) => ((x: ASTExp) => x) }
  )

  lazy val infix6: Parser[ASTExp] = (
    infix7 ~ infix6Tail    ^^ { case (exp ~ tail) => tail(exp) }
  )

  lazy val infix6Tail: Parser[(ASTExp => ASTExp)] = (
      "+" ~ infix6         ^^ { case(_ ~ tail)
          => ((x: ASTExp) =>
                ASTExpInfixApp.leftAssociate(ASTPlusIdent(), x, tail)) }
    | "-" ~ infix6         ^^ { case(_ ~ tail)
          => ((x: ASTExp) =>
                ASTExpInfixApp.leftAssociate(ASTMinusIdent(), x, tail)) }
    | "^" ~ infix6         ^^ { case(_ ~ tail)
          => ((x: ASTExp) =>
                ASTExpInfixApp.leftAssociate(ASTStringCatIdent(), x, tail)) }
    | ""                   ^^ { (_) => ((x: ASTExp) => x) }
  )

  // New level inserted to deal with 'andalso' and 'orelse'
  lazy val infix7: Parser[ASTExp] = (
      "(" ~ exp ~ ")" ~ infix7Tail        ^^ {
          case (_ ~ exp ~ _ ~ tail) => tail(exp)
      }
    // The ordering of these two is important! ID's tend to eat
    // some constants (depite my best efforts)
    | con ~ infix7Tail                    ^^ {
          case (con ~ infix7Tail) => infix7Tail(ASTExpConst(con)) }
    | longid ~ infix7Tail                 ^^ { case (id ~ infix7Tail)
        => id match {
            case ASTLongIdent(Nil) => unreachable
            case ASTLongIdent(id :: Nil) => infix7Tail(ASTExpIdent(id))
            case ASTLongIdent(ids) =>
              infix7Tail(ASTExpIdent(ASTLongIdent(ids)))
        }
    }
    | id ~ infix7Tail                     ^^ {
          case (exp ~ tail) => tail(ASTExpIdent(exp))
    }
    | "let" ~ decs ~ "in" ~ expSeq ~ "end" ~ infix7Tail ^^ {
          case (_ ~ decs ~ _ ~ seq ~ _ ~ infix7Tail) =>
             infix7Tail(ASTExpLetIn(decs, seq)) }
    // Note that () and [] are treated as special values are so
    // are not considered as part of these expressions.
    // A single bracketing has to be treated as a special case
    | "(" ~ expSeq ~ ")" ~ infix7Tail      ^^ {
        case (_ ~ exp ~ _ ~ infix7Tail) =>
          infix7Tail(ASTExpSeq(exp)) }
    | "(" ~ expTuple ~ ")" ~ infix7Tail    ^^ {
        case (_ ~ exp ~ _ ~ infix7Tail) =>
          infix7Tail(ASTExpTuple(exp)) }
    // We use this rather than expTuple because expList allows
    // for singleton lists whereas expTuple excludes tuples
    // of size 1
    // ExpList elements of size 1 are treated as a special case, because
    // they cause an expoenetial blowup if they are allowed
    // to proceed into the expList code.
    | "[" ~ exp ~ "]" ~ infix7Tail         ^^ {
        case (_ ~ exp ~ _ ~ infix7Tail) =>
          infix7Tail(ASTExpList(List(exp)))
    }
    | "[" ~ expList ~ "]" ~ infix7Tail     ^^ {
        case (_ ~ exp ~ _ ~ infix7Tail) =>
          infix7Tail(ASTExpList(exp))
    }
  )

  lazy val infix7Tail: Parser[(ASTExp => ASTExp)] = (
      "*" ~ infix7         ^^ { case (_ ~ tail)
          => ((x: ASTExp) =>
                  ASTExpInfixApp.leftAssociate(ASTTimesIdent(), x, tail)) }
    | "/" ~ infix7         ^^ { case (_ ~ tail)
          => ((x: ASTExp) =>
                  ASTExpInfixApp.leftAssociate(ASTRealDivIdent(), x, tail)) }
    | "mod" ~ infix7       ^^ { case (_ ~ tail)
          => ((x: ASTExp) =>
                  ASTExpInfixApp.leftAssociate(ASTModIdent(), x, tail)) }
    | "div" ~ infix7       ^^ { case (_ ~ tail)
          => ((x: ASTExp) =>
                  ASTExpInfixApp.leftAssociate(ASTIntDivIdent(), x, tail)) }
    | expTail              ^^ { (tail) => ((x : ASTExp) => tail(x)) }
  )

  lazy val expTail: Parser[ASTExp => ASTExp] = (
      "(" ~ exp ~ ")" ~ expTail ^^ {
      case (_ ~ app ~ _ ~ tail) => (fun: ASTExp) => 
          // In this case, we do not push the function application all the way
          // in since.
          tail(ASTExpFunApp(fun, app))
    }
    | exp               ^^ { case (app) => (fun: ASTExp) => { app match {
          case app @ ASTExpFunApp(function, application) =>
            app.leftAssociate(fun)
          case value => ASTExpFunApp(fun, value)
        }
      }
    }
    | ":" ~ typ         ^^ { case (_ ~ typ) =>
          ((exp: ASTExp) => ASTExpTyped(exp, typ)) }
    | ""                ^^ { (_) => (x: ASTExp) => x }
  )

  lazy val expTuple: Parser[List[ASTExp]] = (
    exp ~ "," ~ expList        ^^ {
      case (e ~ _ ~ list) => e :: list
    }
  )

  lazy val expList: Parser[List[ASTExp]] = (
    exp ~ "," ~ expList        ^^ { case (e ~ _ ~ list) => e :: list }
    | exp                      ^^ { (x) => List(x) }
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
                ASTExpMatchRow(List(pat), exp) :: rest }
    | pat ~ "=>" ~ exp                      ^^ { case (pat ~ _ ~ exp) =>
                List(ASTExpMatchRow(List(pat), exp)) }
  )

  // Patterns
  
  // Restructured to avoid left recursion.
  lazy val pat: Parser[ASTPat] = (
      con ~ patTail                     ^^ { case (con ~ patTail)
            => patTail._1(ASTPatConst(con, patTail._2)) }
    | "_" ~ patTail                     ^^ { case (_ ~ patTail)
            => patTail._1(ASTPatWildcard(patTail._2)) }
    // This is inserted to avoid creating an empty patseq below
    // (which causes problems later)
    | "()" ~ patTail                    ^^ { case (_ ~ tail)
            => tail._1(ASTPatVariable(ASTUnitIdent(), tail._2)) }
    // Restricted here as special characters may not appear
    // in pat lists.
    | restrictedIDAllowList  ~ patTail  ^^ { case (id ~ patTail)
            => patTail._1(ASTPatVariable(id, patTail._2)) }
    // Omitted: (op) longid (pat); construction
    // Omitted: (pat) id (pat)
    // Special case: pat id pat -> pat :: pat.
    // Restructured: (pat1, ... patN) replaced with (patlist)
    // Restructured: (pat) replaced with (patList)
    // Resturctured: Single element patterns are treated on their own
    // since they result in an exponential blowup if they are not.
    | "(" ~ pat ~ ")" ~ patTail         ^^ {
          case (_ ~ pat ~ _ ~ patTail) =>
                   patTail._1(ASTPatSeq(List(pat), patTail._2)) }
    | "(" ~ patList ~ ")" ~ patTail     ^^ {
          case (_ ~ ASTPatSeq(patList, _) ~ _ ~ patTail) =>
                   patTail._1(ASTPatSeq(patList, patTail._2)) }
    // Note that there is a distinction between ASTPatSeq and
    // ASTListPat. The former is a pattern list and the later
    // is a list of patterns.
    // Again, single element lists are treated as special cases
    // as they result in an exponential blowup if allowed to continue
    // past here.
    | "[" ~ pat ~ "]" ~ patTail     ^^ {
          case (_ ~ pat ~ _ ~ patTail) => 
                   patTail._1(ASTListPat(List(pat), patTail._2)) }
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
       ((typ: ASTType) => typTail(ASTFunctionType(typ, typ1))) }
    |  "*" ~ typ ~ typTail    ^^ { case (_ ~ typ1 ~ typTail) =>
       ((typ: ASTType) => typ match {
         case (ASTTupleType(tail)) => typTail(ASTTupleType(typ :: tail))
         case (otherTyp) => typTail(ASTTupleType(List(typ, typ1)))
       })
    }
    |  ""                     ^^ { _ => ((x: ASTType) => x) }
  )

  // Omitted: typrow

  //  Declarations

  lazy val dec: Parser[ASTDeclaration] = (
    // Omitted: val (val)(,) valbind
      "val" ~ valbind                ^^ { case (_ ~ valbind) => valbind }
    // Omitted: val (val)(,) funbind
    | "fun" ~ funbind                ^^ { case (_ ~ funbind) => funbind }
    // Omitted: type typebind
    // Omitted: (with typebind)
    | "datatype" ~ datbind           ^^ { case (_ ~ datbind) => datbind }
    // Omitted: abstype datbind (withtype typbind) with dec end
    // Omitted: exnbind
    // Omitted: structure
    // Omitted: local dec1 in dec2 end
    // Omitted: open longid1 ... longidN
    // Omitted nonfix id1 ... idN
    // Omitted infix id1 ... idN
    // Omitted infixr id1 ... idN
  )

  // Inserted: This pattern is inserted to avoid left recursion
  // in dec.
  lazy val decs: Parser[List[ASTDeclaration]] = (
    dec ~ decs                  ^^ { case (hd ~ tail) => hd :: tail }
    | ";"                       ^^ { (_) => List[ASTDeclaration]() }
    | ""                        ^^ { (_) => List[ASTDeclaration]() }
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

  lazy val valIDs: Parser[ASTIdentTuple] = (
      "(" ~> restrictedIDList <~ ")"
    | restrictedValID                    ^^ { (id) => ASTIdentTuple(List(id)) }
  )

  lazy val restrictedIDList: Parser[ASTIdentTuple] = (
      "(" ~ restrictedIDList ~ ")" ~ "," ~ restrictedIDList ^^ {
            case (_ ~ innerTuple ~ _ ~ _ ~ ASTIdentTuple(rest)) =>
              ASTIdentTuple(innerTuple :: rest)
      }
      | "(" ~> restrictedIDList <~ ")"             ^^ { case (innerTuple) =>
              ASTIdentTuple(List(innerTuple))
      }
      | restrictedValID ~ "," ~ restrictedIDList   ^^ {
            case (id ~ _ ~ ASTIdentTuple(idList)) =>
              ASTIdentTuple(id :: idList)
      }
      | restrictedValID                            ^^ { case (id) =>
              ASTIdentTuple(List(id))
      }
  )

  lazy val restrictedValID: Parser[ASTIdent] = (
      "_"                 ^^ { (_) => ASTUnderscoreIdent() }
    | restrictedID
  )

  lazy val funbind: Parser[ASTDeclaration] = (
    funmatch
    //  Omitted funmatch and funbind
  )

  lazy val funmatch: Parser[ASTFunBind] = (
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
    decs ~ "\0" ^^ { case (decs ~ _) => decs }
  )

  override def treeToString(input: ASTProgram) =
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
        new ASTProgram(tree)
      }
    }
  }
}
