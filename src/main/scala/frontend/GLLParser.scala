package frontend

import exceptions.ICE
import java.math.BigDecimal
import java.math.BigInteger
import lexer._
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import toplev.Pass

/* This file contains the parser description.  It has been
 * adapted from: https://people.mpi-sws.org/~rossberg/sml.html
 *
 * Refrences to headers correspond to that document
 * as do references to Omitted, Refactored etc.
 */

/* Known issues:
 *    Nested comments are currently not supported. They probably should
 *    be.
 *
 *    Type annotations on patterns are currently not supported. They probably
 *    should be.
 *
 *    There are issues with the prceedence of 'andalso' and 'orelse'. They
 *    should be lower precedence than the other infix operators, but
 *    are treated equally.
 */

object GLLParser extends Pass[LexemeSeq, ASTProgram]("ast")
    with Parsers {
  override type Elem = Lexeme

  lazy val charLiteral: Parser[LexCharLiteral] =
    accept("char literal", { case (x: LexCharLiteral) => x })

  lazy val floatLiteral: Parser[LexFloatLiteral] =
    accept("float literal", { case (x: LexFloatLiteral) => x })

  lazy val intLiteral: Parser[LexIntLiteral] =
    accept("int literal", { case (x: LexIntLiteral) => x })

  lazy val longIdentifier: Parser[LexLongIdentifier] =
    accept("long identifier", { case (x: LexLongIdentifier) => x })

  lazy val lexIdentifier: Parser[LexIdentifier] =
    accept("identifier", { case (x: LexIdentifier) => x})

  lazy val stringLiteral: Parser[LexStringLiteral] =
    accept("string literal", { case (x: LexStringLiteral) => x })

  lazy val unconstrainedType: Parser[LexUnconstrainedType] =
    accept("unconstraint tyvar", { case (x: LexUnconstrainedType) => x })

  lazy val equalityType: Parser[LexEqualityType] =
    accept("equality  tyvar", { case (x: LexEqualityType) => x })

  // Constants
  lazy val con: Parser[ASTConst] = (
    // Omitted: word
      floatLiteral          ^^ {
        case LexFloatLiteral(value) => ASTConstFloat(value)
    }
    | intLiteral            ^^ {
      case LexIntLiteral(value) => ASTConstInt(value)
    }
    | charLiteral           ^^ {
      case LexCharLiteral(value) => ASTConstChar(value)
    }
    | stringLiteral         ^^ {
      case LexStringLiteral(string) => ASTConstString(string)
    }
    | LexBoolTrue           ^^ { (_) => ASTConstTrue() }
    | LexBoolFalse          ^^ { (_) => ASTConstFalse() }
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
      LexUnit               ^^ { (_) => ASTUnitIdent() }
    | restrictedIDAllowList
  )

  lazy val restrictedIDAllowList: Parser[ASTIdent] = (
      LexNil                ^^ { (_) => ASTEmptyListIdent() }
    | restrictedID
  )

  lazy val restrictedID: Parser[ASTIdent] = (
    lexIdentifier           ^^ {
      case (LexIdentifier(name)) => ASTIdentVar(name)
    }
  )

  lazy val longid: Parser[ASTLongIdent] = (
    longIdentifier          ^^ {
      case LexLongIdentifier(names) =>
        ASTLongIdent(names.map(new ASTIdentVar(_)))
    }
  )

  // Inserted, UnOP
  lazy val unOp: Parser[ASTUnOp] = (
      LexNeg                ^^ { (_) => ASTUnOpNegate() }
    | LexNot                ^^ { (_) => ASTUnOpNot() }
    | LexPrint              ^^ { (_) => ASTUnOpPrint() }
  )

  // Restructure: rename var to tyvar as var is a keyword.
  lazy val tyvar: Parser[ASTTypeVar] = (
      unconstrainedType ~ tyvarTail ^^ {
        case (LexUnconstrainedType(name) ~ tail) =>
          tail(ASTUnconstrainedTypeVar(name))
      }
    | equalityType ~ tyvarTail ^^ {
        case (LexEqualityType(name) ~ tail) =>
                tail(ASTEqualityTypeVar(name))
    }
    // These are inserted as the grammar has omitted them.
    | LexIntType ~ tyvarTail ^^ { case (_ ~ tail) =>
                tail(ASTIntType())
    }
    | LexRealType ~ tyvarTail ^^ { case (_ ~ tail) =>
                tail(ASTRealType())
    }
    | LexStringType ~ tyvarTail ^^ { case (_ ~ tail) =>
                tail(ASTStringType())
    }
    | LexCharType ~ tyvarTail ^^ { case (_ ~ tail) =>
                tail(ASTCharType())
    }
    | LexBoolType ~ tyvarTail ^^ { case (_ ~ tail) =>
                tail(ASTBoolType())
    }
    | LexUnitType ~ tyvarTail ^^ { case (_ ~ tail) =>
                tail(ASTUnitType())
    }
    // Refactored: Lists are the tail here because they are
    // left recursive
    | lexIdentifier ~ tyvarTail ^^ {
        case (LexIdentifier(name) ~ tail) => tail(ASTDataTypeName(name))
    }
  )

  lazy val tyvarTail: Parser[ASTTypeVar => ASTTypeVar] = (
    opt(LexListType ~> tyvarTail) ^^ {
      case Some(tail) => ((ty: ASTTypeVar) => tail(ASTListType(ty)))
      case None => ((x: ASTTypeVar) => x)
    }
  )

  // Omitted: letter, digit. They cause issues with the lexing and
  // are better regex'd

  // Expressions
  // Refactored to avoid left recursion.
  lazy val exp: Parser[ASTExp] = (
      infixApp ~ opt(infix8Tail) ^^ {
        case (infix ~ Some(expTail)) => expTail(infix)
        case (infix ~ None) => infix
      }
    // Refactored: (exp1 dots expn) replaced by the above
    // Omitted: raise exp
    // Omitted: exp handle match
    // Refactored: exp1; exp2; .. expN; replaced by expSeq
    // Note that none of these have EXP tails. This is intentional
    // as I do not believe that that is a meaningful grammar.
    // This may be subject to change.
    // Note that since 'else' is associated with all ifs, this avoids
    // the dangling else ambiguity.
    | LexIf ~ exp ~ LexThen ~ exp ~ LexElse ~ exp ^^ {
          case (_ ~ cond ~ _ ~ taken ~ _ ~ notTaken)
            => ASTExpIfThenElse(cond, taken, notTaken)
    }
    // Rename  match to matchPat since match is a keyword in Scala
    | LexCase ~ exp ~ LexOf ~ matchPat ^^ { case (_ ~ exp ~ _ ~ matchPat)
            => ASTExpCase(exp, matchPat) }
    | LexFn ~ matchPat                 ^^ { case (_ ~ body) => ASTExpFn(body) }
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
        case (exp ~ tail) => tail(exp)
    }
  )

  lazy val infixTopTail: Parser[(ASTExp => ASTExp)] = (
      LexOrElse ~ infixTop    ^^ {
        case (_ ~ tail) => ((x: ASTExp) =>
            ASTExpInfixApp(ASTOrIdent(), x, tail))
      }
    | LexAndAlso ~ infixTop   ^^ {
        case (_ ~ tail) => ((x: ASTExp) =>
            ASTExpInfixApp(ASTAndIdent(), x, tail))
    }
    | opt(infix8Tail)         ^^ {
        case (Some(tail)) => ((x: ASTExp) => tail(x))
        case None => ((x: ASTExp) => x)
    }
  )

  lazy val infix4: Parser[ASTExp] = (
    infix5 ~ opt(infix4Tail)  ^^ {
      case (exp ~ Some(tail)) => tail(exp)
      case (exp ~ None) => exp
    }
  )

  lazy val infix4Tail: Parser[(ASTExp => ASTExp)] = (
      LexLEQ ~ infix4       ^^ {
        case(_ ~ tail) => ((x: ASTExp) =>
          ASTExpInfixApp.leftAssociate(ASTLEQIdent(), x, tail))
      }
    | LexGEQ ~ infix4       ^^ {
        case(_ ~ tail) => ((x: ASTExp) =>
          ASTExpInfixApp.leftAssociate(ASTGEQIdent(), x, tail))
    }
    | LexLT  ~ infix4       ^^ {
        case(_ ~ tail) => ((x: ASTExp) =>
          ASTExpInfixApp.leftAssociate(ASTLTIdent(), x, tail))
    }
    | LexGT  ~ infix4       ^^ {
        case(_ ~ tail) => ((x: ASTExp) =>
          ASTExpInfixApp.leftAssociate(ASTGTIdent(), x, tail))
    }
    | LexEq  ~ infix4       ^^ {
        case(_ ~ tail) => ((x: ASTExp) =>
          ASTExpInfixApp.leftAssociate(ASTEqIdent(), x, tail))
    }
    | LexNeq ~ infix4       ^^ {
        case(_ ~ tail) => ((x: ASTExp) =>
          ASTExpUnOpApply(ASTUnOpNot(),
                          ASTExpInfixApp.leftAssociate(ASTEqIdent(), x, tail)))
    }
  )

  lazy val infix5: Parser[ASTExp] = (
    infix6 ~ opt(infix5Tail) ^^ {
      case (exp ~ Some(tail)) => tail(exp)
      case (exp ~ None) => exp
    }
  )

  lazy val infix5Tail: Parser[(ASTExp => ASTExp)] = (
      LexCons ~ infix5      ^^ {
        case(_ ~ tail) =>
          ((x: ASTExp) => ASTExpInfixApp(ASTConsIdent(), x, tail))
      }
    | LexAppend ~ infix5    ^^ {
        case(_ ~ tail) =>
          ((x: ASTExp) => ASTExpInfixApp(ASTAppendIdent(), x, tail))
    }
  )

  lazy val infix6: Parser[ASTExp] = (
    infix7 ~ opt(infix6Tail) ^^ {
      case (exp ~ Some(tail)) => tail(exp)
      case (exp ~ None) => exp
    }
  )

  lazy val infix6Tail: Parser[(ASTExp => ASTExp)] = (
      LexPlus ~ infix6      ^^ {
        case(_ ~ tail) => ((x: ASTExp) =>
          ASTExpInfixApp.leftAssociate(ASTPlusIdent(), x, tail))
      }
    | LexMinus ~ infix6     ^^ {
        case(_ ~ tail) => ((x: ASTExp) =>
          ASTExpInfixApp.leftAssociate(ASTMinusIdent(), x, tail))
    }
    | LexStringCat ~ infix6 ^^ {
        case(_ ~ tail) => ((x: ASTExp) =>
          ASTExpInfixApp.leftAssociate(ASTStringCatIdent(), x, tail))
    }
  )

  lazy val infix7: Parser[ASTExp] = (
    infix8 ~ opt(infix7Tail) ^^ {
      case (exp ~ Some(tail)) => tail(exp)
      case (exp ~ None) => exp
    }
  )

  lazy val infix7Tail: Parser[(ASTExp => ASTExp)] = (
      LexTimes ~ infix7     ^^ {
        case (_ ~ tail) => ((x: ASTExp) =>
            ASTExpInfixApp.leftAssociate(ASTTimesIdent(), x, tail))
      }
    | LexRealDiv ~ infix7   ^^ {
        case (_ ~ tail) => ((x: ASTExp) =>
            ASTExpInfixApp.leftAssociate(ASTRealDivIdent(), x, tail))
    }
    | LexMod ~ infix7       ^^ {
        case (_ ~ tail) => ((x: ASTExp) =>
            ASTExpInfixApp.leftAssociate(ASTModIdent(), x, tail))
    }
    | LexIntDiv ~ infix7    ^^ {
        case (_ ~ tail) => ((x: ASTExp) =>
            ASTExpInfixApp.leftAssociate(ASTIntDivIdent(), x, tail))
    }
  )

  // New level inserted to deal with 'andalso' and 'orelse'
  lazy val infix8: Parser[ASTExp] = (
      LexLParen ~ exp ~ LexRParen ~ opt(infix8Tail) ^^ {
        case (_ ~ exp ~ _ ~ Some(tail)) => tail(exp)
        case (_ ~ exp ~ _ ~ None) => exp
      }
    // The ordering of these two is important! ID's tend to eat
    // some constants (depite my best efforts)
    | unOp ~ simpleExp      ^^ {
        case (unop ~ simpleExp) => ASTExpUnOpApply(unop, simpleExp)
    }
    | con ~ opt(infix8Tail) ^^ {
        case (con ~ Some(infix8Tail)) => infix8Tail(ASTExpConst(con))
        case (con ~ None) => ASTExpConst(con)
    }
    | longid ~ opt(infix8Tail) ^^ {
        case (id ~ Some(infix8Tail)) => id match {
            case ASTLongIdent(Nil) => unreachable
            case ASTLongIdent(id :: Nil) => infix8Tail(ASTExpIdent(id))
            case ASTLongIdent(ids) =>
              infix8Tail(ASTExpIdent(ASTLongIdent(ids)))
        }
        case (id ~ None) => ASTExpIdent(id)
    }
    | id ~ opt(infix8Tail)  ^^ {
        case (exp ~ Some(tail)) => tail(ASTExpIdent(exp))
        case (exp ~ None) => ASTExpIdent(exp)
    }
    | expLetIn ~ opt(infix8Tail) ^^ {
        case (letIn ~ Some(infix8Tail)) => infix8Tail(letIn)
        case (letIn ~ None) => letIn
    }
    // Note that () and [] are treated as special values are so
    // are not considered as part of these expressions.
    // A single bracketing has to be treated as a special case
    | LexLParen ~ expSeq ~ LexRParen ~ opt(infix8Tail) ^^ {
        case (_ ~ exp ~ _ ~ Some(infix8Tail)) => infix8Tail(ASTExpSeq(exp))
        case (_ ~ exp ~ _ ~ None) => ASTExpSeq(exp)
    }
    | LexLParen ~ expTuple ~ LexRParen ~ opt(infix8Tail) ^^ {
        case (_ ~ exp ~ _ ~ Some(infix8Tail)) => infix8Tail(ASTExpTuple(exp))
        case (_ ~ exp ~ _ ~ None) => ASTExpTuple(exp)
    }
    // We use this rather than expTuple because expList allows
    // for singleton lists whereas expTuple excludes tuples
    // of size 1
    // ExpList elements of size 1 are treated as a special case, because
    // they cause an expoenetial blowup if they are allowed
    // to proceed into the expList code.
    | LexLBrack ~ exp ~ LexRBrack ~ opt(infix8Tail) ^^ {
        case (_ ~ exp ~ _ ~ Some(infix8Tail)) =>
          infix8Tail(ASTExpList(List(exp)))
        case (_ ~ exp ~ _ ~ None) => ASTExpList(List(exp))
    }
    | LexLBrack ~ expList ~ LexRBrack ~ opt(infix8Tail) ^^ {
        case (_ ~ exp ~ _ ~ Some(infix8Tail)) => infix8Tail(ASTExpList(exp))
        case (_ ~ exp ~ _ ~ None) => ASTExpList(exp)
    }
  )

  lazy val infix8Tail: Parser[ASTExp => ASTExp] = (
      // This is added as a special case, as in this case, if
      // exp is a function application, we do not want to left
      // associate within the function application.
      LexLParen ~ exp ~ LexRParen ~ opt(infix8Tail) ^^ {
        case (_ ~ app ~ _ ~ Some(tail)) =>
          (fun: ASTExp) => tail(ASTExpFunApp(fun, app))
        case (_ ~ app ~ _ ~ None) =>
          (fun: ASTExp) => ASTExpFunApp(fun, app)
      }
    | simpleExp ~ opt(infix8Tail)       ^^ {
      case (app ~ tail) => (fun: ASTExp) => { app match {
          case app @ ASTExpFunApp(function, application) => tail match {
            case Some(tail) => tail(app.leftAssociate(fun))
            case None => app.leftAssociate(fun)
          }
          case value => tail match {
            case Some(tail) => tail(ASTExpFunApp(fun, value))
            case None => ASTExpFunApp(fun, value)
          }
        }
      }
    }
    | LexColon ~ typ              ^^ {
        case (_ ~ typ) => ((exp: ASTExp) => ASTExpTyped(exp, typ))
    }
  )

  lazy val simpleExp: Parser[ASTExp] = (
      id                    ^^ { (id) => ASTExpIdent(id) }
    | con                   ^^ { (con) => ASTExpConst(con) }
    | LexLParen ~ exp ~ LexRParen ^^ { case (_ ~ exp ~ _) => exp }
    | LexLParen ~ expTuple ~ LexRParen ^^ {
        case (_ ~ exp ~ _) => ASTExpTuple(exp)
    }
    | LexLBrack ~ expList ~ LexRBrack ^^ {
        case (_ ~ exp ~ _) => ASTExpList(exp)
    }
  )

  lazy val expLetIn: Parser[ASTExp] =
    LexLet ~ decs ~ LexIn ~ expSeq ~ LexEnd ^^ {
      case (_ ~ decs ~ _ ~ seq ~ _) => ASTExpLetIn(decs, seq)
    }

  lazy val expTuple: Parser[List[ASTExp]] = (
    exp ~ LexComma ~ expList ^^ {
      case (e ~ _ ~ list) => e :: list
    }
  )

  lazy val expList: Parser[List[ASTExp]] = (
    exp ~ LexComma ~ expList ^^ { case (e ~ _ ~ list) => e :: list }
    | exp                   ^^ { (x) => List(x) }
  )

  // This accepts seqs of one or more.
  lazy val expSeq: Parser[List[ASTExp]] = (
      exp ~ rep(LexSemiColon) ~ expSeq ^^ {
        case (exp ~ _ ~ expSeq) => exp :: expSeq
      }
    | exp ~ rep(LexSemiColon)   ^^ { case (e ~ _) => List(e) }
  )

  // Omitted: exprow

  // Renamed to matchPat since match is a keyword in scala
  lazy val matchPat: Parser[List[ASTExpMatchRow]] = (
    // Note that this is not allowed to be empty
    pat ~ LexFnDecArrow ~ exp ~ opt(LexVBar ~ matchPat) ^^ {
      case (pat ~ _ ~ exp ~ Some(_ ~ rest)) =>
        ASTExpMatchRow(List(pat), exp) :: rest
      case (pat ~ _ ~ exp ~ None) => List(ASTExpMatchRow(List(pat), exp))
    }
  )

  // Patterns

  // This is inserted as not all patterns are allowed without brackets.
  // For example, fun f x :: xs is not allowed.  This is needed as a wrapper.
  lazy val patNoType: Parser[ASTPat] = (
      con                     ^^ {
        case (con) => ASTPatConst(con, List())
    }
    | LexUnderscore           ^^ {
      case (_) => ASTPatWildcard(List())
    }
    | LexUnit                 ^^ {
      case (_) => ASTPatVariable(ASTUnitIdent(), List())
    }
    | restrictedIDAllowList   ^^ {
      case (id) => ASTPatVariable(id, List())
    }
    | LexLParen ~ pat ~ LexRParen ^^ {
      case (_ ~ pat ~ _) => pat
    }
    | LexLParen ~ patList ~ LexRParen ^^ {
      case (_ ~ patList ~ _) => patList
    }
    | LexLBrack ~ pat ~ LexRBrack ^^ {
      case (_ ~ pat ~ _) => ASTListPat(List(pat), List())
    }
    | LexLBrack ~ patList ~ LexRBrack ^^ {
      case (_ ~ ASTPatSeq(seq, List()) ~ _) =>
        ASTListPat(seq, List())
    }
  )

  // Restructured to avoid left recursion.
  lazy val pat: Parser[ASTPat] = (
      con ~ opt(patTail)      ^^ {
        case (con ~ Some(patTail)) => patTail._1(ASTPatConst(con, patTail._2))
        case (con ~ None) => ASTPatConst(con, List())
      }
    | LexUnderscore ~ opt(patTail) ^^ {
        case (_ ~ Some(patTail)) => patTail._1(ASTPatWildcard(patTail._2))
        case (_ ~ None) => ASTPatWildcard(List())
    }
    // This is inserted to avoid creating an empty patseq below
    // (which causes problems later)
    | LexUnit ~ opt(patTail) ^^ {
        case (_ ~ Some(tail)) =>
          tail._1(ASTPatVariable(ASTUnitIdent(), tail._2))
        case (_ ~ None) => ASTPatVariable(ASTUnitIdent(), List())
    }
    // Restricted here as special characters may not appear
    // in pat lists.
    | restrictedIDAllowList  ~ opt(patTail) ^^ {
        case (id ~ Some(patTail)) => patTail._1(ASTPatVariable(id, patTail._2))
        case (id ~ None) => ASTPatVariable(id, List())
    }
    // Omitted: (op) longid (pat); construction
    // Omitted: (pat) id (pat)
    // Special case: pat id pat -> pat :: pat.
    // Restructured: (pat1, ... patN) replaced with (patlist)
    // Restructured: (pat) replaced with (patList)
    // Resturctured: Single element patterns are treated on their own
    // since they result in an exponential blowup if they are not.
    | LexLParen ~ pat ~ LexRParen ~ opt(patTail) ^^ {
        case (_ ~ pat ~ _ ~ Some(patTail)) =>
          patTail._1((pat.appendTypes(patTail._2)))
        case (_ ~ pat ~ _ ~ None) => pat
    }

    | LexLParen ~ patList ~ LexRParen ~ opt(patTail) ^^ {
        case (_ ~ ASTPatSeq(patList, _) ~ _ ~ Some(patTail)) =>
          patTail._1(ASTPatSeq(patList, patTail._2))
        case (_ ~ patSeq ~ _ ~ None) =>
          patSeq
    }
    // Note that there is a distinction between ASTPatSeq and
    // ASTListPat. The former is a pattern list and the later
    // is a list of patterns.
    // Again, single element lists are treated as special cases
    // as they result in an exponential blowup if allowed to continue
    // past here.
    | LexLBrack ~ pat ~ LexRBrack ~ opt(patTail) ^^ {
        case (_ ~ pat ~ _ ~ Some(patTail)) =>
          patTail._1(ASTListPat(List(pat), patTail._2))
        case (_ ~ pat ~ _ ~ None) =>
          ASTListPat(List(pat), List())
    }
    | LexLBrack ~ patList ~ LexRBrack ~ opt(patTail) ^^ {
        case (_ ~ ASTPatSeq(patSeq, _) ~ _ ~ Some(patTail)) =>
          patTail._1(ASTListPat(patSeq, patTail._2))
        case (_ ~ ASTPatSeq(patSeq, _) ~ _ ~ None) =>
          ASTListPat(patSeq, List())
    }
    // Omitted: (op) id (:typ) as pat; layed
  )

  lazy val patTail: Parser[((ASTPat => ASTPat), List[ASTType])] = (
      LexCons ~ pat            ^^ { case (_ ~ pat) =>
        ((prePat: ASTPat) =>
          ASTPatCons(prePat, pat, List[ASTType]()), List[ASTType]())
    }
    | LexColon ~ typ ~ opt(typList) ^^ {
        case (_ ~ typ ~ Some(typList)) =>
          (((x: ASTPat) => x), (typ :: typList))
        case (_ ~ typ ~ None) => ((x: ASTPat) => x, List(typ))
    }
  )

  lazy val typList: Parser[List[ASTType]] = (
      LexColon ~ typ ~ opt(typList) ^^ {
        case (_ ~ typ ~ Some(typList)) => typ :: typList
        case (_ ~ typ ~ None) => List(typ)
      }
  )

  // Note that the second argument of ASTPatSeq is a list of
  // types that is assigned to this sequence (as a WHOLE)
  lazy val patList: Parser[ASTPatSeq] = (
      pat ~ opt(LexComma ~ patList) ^^ {
        case (pat ~ Some(_ ~ ASTPatSeq(list, Nil))) =>
          ASTPatSeq(pat :: list, Nil)
        case (pat ~ None) =>
          ASTPatSeq(List(pat), Nil)
      }
  )

  // Omitted: patrow

  // Types

  lazy val typ: Parser[ASTType] = (
    // Restructure: Rename var to tyvar as var is a keyword.
    // Restructure: use typTail to avoid ambiguity
    tyvar ~ opt(typTail)    ^^ {
        case (tyvar ~ Some(tail)) => tail(tyvar)
        case (tyvar ~ None) => tyvar
    }
    // Omitted: (typ)(,) longid; constructor
    | LexLParen ~ typ ~ LexRParen ~ opt(typTail) ^^ {
        case (_ ~ typ ~ _ ~ Some(typTail)) => typTail(typ)
        case (_ ~ typ ~ _ ~ None) => typ
    }
    // Omitted: { (typrow) }; record
  )

  lazy val typTail: Parser[(ASTType => ASTType)] = (
    LexFunType ~ typ ~ opt(typTail) ^^ {
      case (_ ~ typ1 ~ Some(typTail)) =>
        ((typ: ASTType) => ASTFunctionType(typ, typTail(typ1)))
      case (_ ~ typ1 ~ None) =>
        ((typ: ASTType) => ASTFunctionType(typ, typ1))
    }
    |  LexTimes ~ typ ~ opt(typTail) ^^ {
      case (_ ~ typ1 ~ Some(typTail)) =>
       ((typ: ASTType) => typ match {
         case ASTTupleType(tail) => typTail(ASTTupleType(typ :: tail))
         case otherTyp => typTail(ASTTupleType(List(typ, typ1)))
       })
      case ( _ ~ typ1 ~ None) =>
        ((typ: ASTType) => typ match {
          case ASTTupleType(tail) => ASTTupleType(typ :: tail)
          case otherTyp => ASTTupleType(List(typ, typ1))
        })
    }
  )

  // Omitted: typrow

  //  Declarations

  lazy val dec: Parser[ASTDeclaration] = (
    // Omitted: val (val)(,) valbind
      LexVal ~ valbind ~ rep(LexSemiColon) ^^ {
        case (_ ~ valbind ~ _) => valbind
      }
    // Omitted: val (val)(,) funbind
    | LexFun ~ funbind ~ rep(LexSemiColon) ^^ {
        case (_ ~ funbind ~ _) => funbind
    }
    // Omitted: type typebind
    // Omitted: (with typebind)
    | LexDatatype ~ datbind ~ rep(LexSemiColon) ^^ {
        case (_ ~ datbind ~ _) => datbind
    }
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
    rep(dec)                ^^ { case (decs) => decs }
  )

  lazy val valbind: Parser[ASTDeclaration] = (
      // Refactor: Replace pat with valIDs to keep some
      // simplicity. Cost is some valid expressions.
      // Restricted as special characters may not appear
      // Refactor:
    valIDs ~ LexEq ~ exp    ^^ {
        case (id ~ _ ~ exp) => ASTValBind(id.flatten, exp) }
      // Omitted pat = exp and valbind
      // Omitted rec valbind
  )

  lazy val valIDs: Parser[ASTIdentTuple] = (
      LexLParen ~> restrictedIDList <~ LexRParen
    | restrictedValID       ^^ { (id) => ASTIdentTuple(List(id)) }
  )

  lazy val restrictedIDList: Parser[ASTIdentTuple] = (
      LexLParen ~ restrictedIDList ~ LexRParen ~
        opt(LexComma ~ restrictedIDList) ^^ {
          case (_ ~ innerTuple ~ _ ~ Some(_ ~ ASTIdentTuple(rest))) =>
            ASTIdentTuple(innerTuple.flatten :: rest)
          case (_ ~ innerTuple ~ _ ~ None) =>
            ASTIdentTuple(List(innerTuple))
      }
      | restrictedValID ~ LexComma ~ restrictedIDList ^^ {
          case (id ~ _ ~ ASTIdentTuple(idList)) =>
              ASTIdentTuple(id :: idList)
      }
      | restrictedValID     ^^ { case (id) =>
              ASTIdentTuple(List(id))
      }
  )

  lazy val restrictedValID: Parser[ASTIdent] = (
      LexUnderscore         ^^ { (_) => ASTUnderscoreIdent() }
    | restrictedID
  )

  lazy val funbind: Parser[ASTDeclaration] = (
    funmatch
    //  Omitted funmatch and funbind
  )

  lazy val funmatch: Parser[ASTFunBind] = (
    // Omitted: (op) prefix
    // Restructured: replace pat1 ... patn with patN parser.
    restrictedID ~ patCurriedNoTypes ~ opt(LexColon ~ typ) ~
      LexEq ~ exp ~ opt(LexVBar ~ funmatch) ^^ {
        case (id ~ pattern ~ Some(_ ~ typ) ~
              _ ~ exp ~ Some(_ ~ ASTFunBind(patList))) =>
          ASTFunBind((id, pattern, Some(typ), exp) :: patList)
        case (id ~ pattern ~ Some(_ ~ typ) ~ _ ~ exp ~ None) =>
          ASTFunBind(List((id, pattern, Some(typ), exp)))
        case (id ~ pattern ~ None ~ _ ~ exp ~ Some(_ ~ ASTFunBind(patList))) =>
          ASTFunBind((id, pattern, None, exp) :: patList)
        case (id ~ pattern ~ None ~ _ ~ exp ~ None) =>
          ASTFunBind(List((id, pattern, None, exp )))
      }
    // Omitted: infix patterns:
    //   pat1 id pat2 (: typ) = exp (funmatch)
    //   (pat1 id pat2) pat`1 ... pat`N (: typ) = exp (| funmatch)
  )

  lazy val patCurriedNoTypes: Parser[List[ASTPat]] = (
    patNoType ~ opt(patCurriedNoTypes) ^^ {
      case (pat ~ Some(patList)) => pat :: patList
      case (pat ~ None) => List(pat)
    }
  )

  lazy val datbind: Parser[ASTDeclaration] = (
    // Omitted: var (,) id = conbind (and datbind)
    // Omitted: var (,) prefix
    restrictedID ~ LexEq ~ conbind ^^ {
      case (id ~ _ ~ conbind) => ASTDataType(id, conbind)
    }
  )

  lazy val conbind: Parser[List[ASTDataConstructor]] = (
    restrictedID ~ opt(LexOf ~ typ) ~ opt(LexVBar ~ conbind) ^^ {
      case (id ~ Some(_ ~ typ) ~ Some(_ ~ rest)) =>
        ASTDataConstructorDefinitionWithType(id, typ) :: rest
      case (id ~ Some(_ ~ typ) ~ none) =>
        List(ASTDataConstructorDefinitionWithType(id, typ))
      case (id ~ None ~ Some(_ ~ rest)) =>
        ASTDataConstructorDefinition(id) :: rest
      case (id ~ None ~ None) => List(ASTDataConstructorDefinition(id))
    }
  )

  // Programs

  lazy val prog: Parser[List[ASTDeclaration]] = (
    phrase(decs)            ^^ { (decs) => decs }
  )

  override def treeToString(input: ASTProgram) =
    input.prettyPrint + "\n\n__ Formatted Version __\n\n" +
    input.toString

  protected def run(code: LexemeSeq) = {
    val lexemeReader = new LexemeReader(code.seq)
    val ast = prog(lexemeReader)

    ast match {
      case Failure(msg, remaining) => {
          println("Syntax Error")
          println(msg)
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
