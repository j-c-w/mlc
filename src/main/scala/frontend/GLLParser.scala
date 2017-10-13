package frontend

import com.codecommit.gll._
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
  override val whitespace = """(\s|\(\*([^*]|\*[^)])*\*\))+"""r

  // Constants

  lazy val con: Parser[ASTConst] = (
      int
    // Omitted: word
    | float
    | char
    | string
    | bool
  )

  lazy val int: Parser[ASTConstInt] = (
    num
    | "~" ~ num     ^^ { case (_, ASTConstInt(int)) =>
                  ASTConstInt(int.negate()) }
    // Omitted: (~)0xhex (hexadecimal)
  )

  // Omitted: word

  lazy val float: Parser[ASTConstFloat] = (
    // Refactored: Replaced ~num with int
      int ~ "." ~ num             ^^ { (int, _, dec) =>
            ASTConstFloat(int, dec, ASTConstInt(new BigInteger(new
            Array[Byte](1)))) }
    | int ~ "e" ~ int             ^^ { (int, _, exp) =>
            ASTConstFloat(int, ASTConstInt(new BigInteger(new Array[Byte](1))),
                          exp) }
    | int ~ "." ~ num ~ "e" ~ int ^^ { (int, _, dec, _, exp) =>
            ASTConstFloat(int, dec, exp) }
  )

  lazy val char: Parser[ASTConstChar] = (
    "#\"" ~ ascii ~ "\""      ^^ { case (_, charList, _) => charList }
  )

  lazy val string: Parser[ASTConstString] = (
      // Refactored: replaced ascii* with asciiSeq
      "\"" ~ asciiSeq ~ "\""  ^^ { case (_, string, _) =>
            string }
      // TODO -- extend the strings accepted
  )

  lazy val ascii: Parser[ASTConstChar] = (
    // Omitted: Escape sequences.
    // This regex matches all single characters
    // except " and \
    """[\]-~ !#-\[]""".r     ^^ { (char) => ASTConstChar(char.charAt(0)) }
  )

  lazy val asciiSeq: Parser[ASTConstString] = (
      ""                       ^^ { (_) => ASTConstString("") }
    | ascii ~ asciiSeq         ^^ { case (ASTConstChar(char),
                                          ASTConstString(str)) =>
          ASTConstString(char + str) }
  )

  lazy val bool: Parser[ASTConstBool] = (
      "true".r      ^^ { (_) => ASTConstTrue() }
    | "false".r      ^^ { (_) => ASTConstFalse() }
  )

  lazy val num: Parser[ASTConstInt] = (
    // Note, may not start with a 0
    """[1-9][0-9]*""".r        ^^ { (int) => ASTConstInt(new BigInteger(int)) }
    | "0"                      ^^ { (_) => ASTConstInt(new BigInteger("0")) }
  )

  // Identifiers
  lazy val id: Parser[ASTIdent] = (
    // Refactored: letter(letter|digit|'|_)* into idTail
    letter ~ idTail    ^^  { (letter, idRest) => ASTIdentVar(letter + idRest) }
    // Since these characters all have special meaning, we 
    // interpret them individually.
    | """!|%||&|$|#|\+|-|/|:|<|=|>|\?|@|\\|~|`|\^|\||\*""".r 
                       ^^  { (x) => ASTIdent.fromSpecialCharacter(x) }
  )

  lazy val longid: Parser[ASTLongIdent] = (
      id                ^^ { (ident) => ASTLongIdent(List(ident)) }
    | id ~ "." ~ longid ^^ { case (ident1, _, ASTLongIdent(rest)) =>
            ASTLongIdent(ident1 :: rest) }
  )

  lazy val idTail: Parser[String] = (
      "" 
    | letter ~ idTail  ^^ { (l, t) => l + t }
    | digit ~ idTail   ^^ { (d, t) => d + t }
    | "'" ~ idTail     ^^ { (c, t) => c + t }
    | "_" ~ idTail     ^^ { (u, t) => u + t }
  )

  // Restructure: rename var to tyvar as var is a keyword.
  lazy val tyvar: Parser[ASTTypeVar] = (
    // Restructure: Avoid ambiguity between unconstrained and
    // equality types.
      "'" ~ tyVarLetterNotDash ~ tyVarString   ^^ { (_, firstLetter, rest) =>
              ASTUnconstrainedTypeVar(firstLetter + rest) }
    | "''" ~ tyVarString                       ^^ { (_, name) =>
              ASTEqualityTypeVar(name) }
  )

  lazy val tyVarString: Parser[String] = (
    ""
    | letter ~ tyVarString  ^^  { case(letter, rest) => letter + rest }
    | digit ~ tyVarString   ^^  { case(letter, rest) => letter + rest }
    | "_" ~ tyVarString     ^^  { case(letter, rest) => letter + rest }
    | "'" ~ tyVarString     ^^  { case(letter, rest) => letter + rest }
  )

  lazy val tyVarLetterNotDash: Parser[String] = (
    // This is inserted to avoid ambiguity
    // of non equality and equality types.
    letter    ^^ { (x) => x.toString() }
    | digit   ^^ { (x) => x.toString() }
    | "_"     ^^ { (x) => x.toString() }
  )

  // Inserted rules to specify "letter" and "digit"
  lazy val letter: Parser[Char] = (
    """[A-Za-z]""".r  ^^ { (x) => x.charAt(0) }
  )

  lazy val digit: Parser[BigInteger] = (
    """[0-9]""".r     ^^ { (x) => new BigInteger(x) }
  )

  // Expressions
  lazy val exp: Parser[ASTExp] = (
      con                    ^^ { (con) => ASTExpConst(con) }
    | longid                 ^^ { (id) => ASTExpIdent(id) }
    | exp ~ id ~ exp         ^^ { (op1, id, op2) =>
            ASTExpInfixApp(id, op1, op2) }
    | exp ~ exp              ^^ { (fun, app) => ASTExpFunApp(fun, app) }
    | "(" ~> exp <~ ")"
    // Refactored: (exp1 dots expn) replaced by expList
    | "(" ~> expList <~ ")"  ^^ { (expList) => ASTExpTuple(expList) }
    // Refactored: exp1; exp2; .. expN; replaced by expSeq
    | "let" ~ dec ~ "in" ~ expSeq ~ "end" ^^ { (_, dec, _, seq, _) =>
            ASTExpLetIn(dec, seq) }
    | exp ~ ":" ~ typ        ^^ { (exp, _, typ) => ASTExpTyped(exp, typ) }
    // Omitted: raise exp
    // Omitted: exp handle match
    | exp ~ "orelse" ~ exp   ^^ { (e1, _, e2) => ASTExpOr(e1, e2) }
    | exp ~ "andalso" ~ exp  ^^ { (e1, _, e2) => ASTExpAnd(e1, e2) }
    // Note that since 'else' is associated with all ifs, this avoids
    // the dangling else ambiguity.
    | "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp ^^ {
            (_, cond, _, taken, _, notTaken) =>
              ASTExpIfThenElse(cond, taken, notTaken)
    }
    // Rename  match to matchPat since match is a keyword in Scala
    | "case" ~ exp ~ "of" ~ matchPat ^^ { (_, exp, _, matchPat) =>
              ASTExpCase(exp, matchPat) }
    | "fn" ~ matchPat        ^^ { (_, body) => ASTExpFn(body) }
  )

  // Inserted: To handle non one sized expression lists
  lazy val expList: Parser[List[ASTExp]] = (
      ""                                         ^^ { _ => List[ASTExp]() }
    | exp ~ "," ~ exp ~ "," ~ anyLengthExpList   ^^ { (e1, _, e2, _, rest) =>
            e1 :: e2 :: rest }
  )

  // Inserted. To deal with the restriction that (epx1, ..., expN)
  // is a tuple iff N != 1 (but can be 0)
  lazy val anyLengthExpList: Parser[List[ASTExp]] = (
      ""                               ^^ { _ => List[ASTExp]() }
    | exp ~ "," ~ anyLengthExpList     ^^ { (e1, _, rest) => e1 :: rest }
  )

  // This accepts seqs of one or more.
  lazy val expSeq: Parser[List[ASTExp]] = (
      exp                ^^ { (e) => List(e) }
    | exp ~ ";" ~ expSeq ^^ { (exp, _, expSeq) => exp :: expSeq }
  )

  // Omitted: exprow

  // Renamed to matchPat since match is a keyword in scala
  lazy val matchPat: Parser[List[ASTExpMatchRow]] = (
    // Note that this is not allowed to be empty
      pat ~ "=>" ~ exp                      ^^ { (pat, _, exp) =>
                List(ASTExpMatchRow(pat, exp)) }
    | pat ~ "=>" ~ exp ~ "|" ~ matchPat     ^^ {
          case (pat, _, exp, _, rest) =>
                ASTExpMatchRow(pat, exp) :: rest }
  )

  // Patterns
  
  lazy val pat: Parser[ASTPat] = (
      con                            ^^ { (con) => ASTPatConst(con) }
    | "_"                            ^^ { (_) => ASTPatWildcard() }
    | id                             ^^ { (id) => ASTPatVariable(id) }
    // Omitted: (op) longid (pat); construction
    // Omitted: (pat) id (pat)
    // Restructured: (pat1, ... patN) replaced with (patlist)
    // Restructured: (pat) replaced with (patList)
    | "(" ~> patList <~ ")"          ^^ { (patList)
                => patList }
    // Note that there is a distinction between ASTPatSeq and
    // ASTListPat. The former is a pattern list and the later
    // is a list of patterns.
    | "[" ~> patList <~ "]"          ^^ { case (ASTPatSeq(patSeq)) => 
                   ASTListPat(patSeq) }
    | pat ~ ":" ~ typ                ^^ { (pat, _, typ) =>
                   ASTPatTyped(pat, typ) }
    // Omitted: (op) id (:typ) as pat; layed
  )

  lazy val patList: Parser[ASTPatSeq] = (
      ""                             ^^ { (_) => ASTPatSeq(Nil) }
    | pat ~ "," ~ patList            ^^ { case (pat, _, ASTPatSeq(list)) =>
                ASTPatSeq(pat :: list) }
    | pat                            ^^ { (pat) => ASTPatSeq(List(pat)) }
  )
  
  // Omitted: patrow

  // Types
  
  lazy val typ: Parser[ASTType] = (
    // Restructure: Rename var to tyvar as var is a keyword.
      tyvar
    // Omitted: (typ)(,) longid; constructor
    | "(" ~> typ <~ ")"
    | typ ~ "->" ~ typ    ^^ { (typ1, _, typ2) => ASTTypeFunction(typ1, typ2) }
    // Refactored. Replaced typ1 * ... * typN with typTuple
    | typTuple
    // Omitted: { (typrow) }; record
  )

  // Use this in preference to typList as this only accepts
  // list of length >= 2, which avoids ambiguity of infinite nesting
  // depth of these.
  //
  // There is ambiguity in the order of generation of the tuple,
  // but GLL resolves that after it turns out they are all the same.
  lazy val typTuple: Parser[ASTTypeTuple] = (
    // Refactor: use typList to get possibly empty
    // tuples
    typ ~ "*" ~ typ ~ "*" ~ typList     ^^ { 
      case (typ1, _, typ2, _, ASTTypeTuple(typList)) =>
          ASTTypeTuple(typ1 :: typ2 :: typList) }
  )

  lazy val typList: Parser[ASTTypeTuple] = (
      ""                     ^^  { (_) => ASTTypeTuple(Nil) }
    | typ ~ "*" ~ typList    ^^  {
        case (typ, _, ASTTypeTuple(typList)) =>
          ASTTypeTuple(typ :: typList)
    }
  )

  // Omitted: typrow

  //  Declarations

  lazy val dec: Parser[List[ASTDeclaration]] = (
    // Omitted: val (val)(,) valbine
      "val" ~ valbind                ^^ { (_, valbind) => List(valbind) }
    // Omitted: val (val)(,) funbind
    | "fun" ~ funbind                ^^ { (_, funbind) => List(funbind) }
    // Omitted: type typebind
    // Omitted: (with typebind)
    | "datatype" ~ datbind           ^^ { (_, datbind) => List(datbind) }
    // Omitted: abstype datbind (withtype typbind) with dec end
    // Omitted: exnbind
    // Omitted: structure
    | dec ~ dec                      ^^ { (list1, list2) => 
                                              list1 ::: list2 }
    // Omitted: local dec1 in dec2 end
    // Omitted: open longid1 ... longidN
    // Omitted nonfix id1 ... idN
    // Omitted infix id1 ... idN
    // Omitted infixr id1 ... idN
  )

  lazy val valbind: Parser[ASTDeclaration] = (
      // Refactor: Replace pat with id
      id ~ "=" ~ exp  ^^ { (id, _, exp) => ASTValBind(id, exp) }
      // Omitted pat = exp and valbind
      // Omitted rec valbind
  )

  lazy val funbind: Parser[ASTDeclaration] = (
    funmatch
    //  Omitted funmatch and funbind
  )

  lazy val funmatch: Parser[ASTFun] = (
    // Omitted: (op) prefix
    // Restructured: replace pat1 ... patn with patN parser.
      id ~ patN ~ "=" ~ exp                          ^^ { (id, pattern, _, exp)
            => ASTFunBind(List((id, pattern, None, exp))) }
    | id ~ patN ~ ":" ~ typ ~ "=" ~ exp                ^^ {
          (id, pattern, _, typ, _, exp)
            => ASTFunBind(List((id, pattern, Some(typ), exp))) }
    | id ~ patN ~ "=" ~ exp ~ "|" ~ funmatch             ^^ {
          case (id, pattern, _, exp, _, ASTFunBind(patList))
            => ASTFunBind((id, pattern, None, exp) :: patList) }
    | id ~ patN ~ ":" ~ typ ~ "=" ~ exp ~ "|" ~ funmatch ^^ {
          case (id, pattern, _, typ, _, exp, _, ASTFunBind(patList))
            => ASTFunBind((id, pattern, Some(typ), exp) :: patList) }
    // Omitted: infix patterns:
    //   pat1 id pat2 (: typ) = exp (funmatch)
    //   (pat1 id pat2) pat`1 ... pat`N (: typ) = exp (| funmatch)
  )

  lazy val patN: Parser[List[ASTPat]] = (
      pat             ^^ { (pat) => List(pat) }
    | pat ~ patN      ^^ { (pat, patList) => pat :: patList }
  )

  lazy val datbind: Parser[ASTDeclaration] = (
    // Omitted: var (,) id = conbind (and datbind)
    // Omitted: var (,) prefix
    id ~ "=" ~ conbind                         ^^ { (id, _, conbind)
          => ASTDataType(id, conbind) }
  )

  lazy val conbind: Parser[List[ASTDataConstructor]] = (
    id                                         ^^ { x =>
            List(ASTDataConstructorDefinition(x)) }
    | id ~ "of" ~ typ                          ^^ { (id, _, typ)
            => List(ASTDataConstructorDefinitionWithType(id, typ)) }
    | id ~ "of" ~ typ ~ "|" ~ conbind          ^^ { (id, _, typ, _, rest)
            => ASTDataConstructorDefinitionWithType(id, typ) :: rest }
    | id ~ "|" ~ conbind                       ^^ { (id, _, rest)
            => ASTDataConstructorDefinition(id) :: rest }
  )

  // Programs

  lazy val prog: Parser[List[ASTDeclaration]] = (
    dec
    | ""   ^^ { (_) => List[ASTDeclaration]() }
  )

  def treeToString(input: ASTProgram) =
    input.prettyPrint

  protected def run(code: String) = {
    val allASTs = prog(code)

    val successes = allASTs.filter(_.isInstanceOf[Success[ASTProgram]])
    val failures = allASTs.filter(_.isInstanceOf[Failure])

    if (failures.length > 0) {
      println("Syntax Error")

      failures.foreach({
        case Failure(msg, tail) => {
          val pattern = "  error:%%d: %s%n    %%s%n    %%s%n".format(msg)
          tail.printError(pattern)(System.out)
        }
        case Success(_, _) => unreachable
      })
      System.exit(1)
    }

    // Extract the tree  from the sucessful parsing
    val treeStream = for (Success(tree, _) <- allASTs) yield {
      new ASTProgram(tree)
    }

    // GLLParser returns a stream of all possible interpretations.
    // We expect the length of that list to be 1, as this should not
    // be ambiguous.
    if (allASTs.length != 1) {
      println("Error: Ambiguous Grammar. Please submit the source as" +
        " a bug report")
      println("Found " + allASTs.length.toString + " possible trees")
      println(treeStream.map(_.prettyPrint).mkString("\n\n\n"))
      System.exit(1)
    }

    treeStream(0)
  }
}
