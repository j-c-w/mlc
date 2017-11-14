package lower_ast

import exceptions.{BadIntException,ICE}
import scala.collection.mutable.HashMap
import toplev.Pass
import java.math.BigInteger
import toplev.GenericTypeEnv
import toplev.Shared

import frontend._
import tir._

object LowerAST extends Pass[ASTProgram, TProgram]("lower_ast") {
  def lowerAST(program: ASTProgram): TProgram = {
    val ttypeEnv = lowerEnv(program.env.get)
    val (funDecs, valDecs) = lowerAST(program.decs, program.env.get)

    TProgram(ttypeEnv, funDecs, valDecs)
  }

  def lowerAST(program: List[ASTDeclaration],
               env: ASTTypeEnv): (List[TFun], List[TVal]) = {
    var funDecs = List[TFun]()
    var valDecs = List[TVal]()

    for (dec <- program) {
      dec match {
        case funbind @ ASTFunBind(cases) => {
          val tIdent =  cases(0)._1 match {
            case ASTIdentVar(name) => TIdentVar(name)
            case _ => unreachable
          }

          // We first zip up the cases into a list of ASTMatchRows:
          val matchRows = (cases zip funbind.rowEnvs.get) map {
            case ((_, pattern, _, expression), rowEnv) => {
              val row = ASTExpMatchRow(pattern, expression)
              row.env = Some(rowEnv)
              row
            }
          }

          funDecs =
            TFun(tIdent, matchRows.map(lowerMatchRowAST(_, env))) :: funDecs
        }
        case ASTValBind(ident, expression) => {
          val newIdents = lowerAST(ident, env)
          val newExpression = lowerAST(expression, env)

          valDecs = TVal(newIdents, newExpression) :: valDecs
        }
        case _ =>
          // Datatypes may get down to here. They are formally
          // pruned out of the tree at this point, although
          // as  is they do not pass typechecking.
          ???
      }
    }
    (funDecs.reverse, valDecs.reverse)
  }

  def lowerAST(ident: ASTIdent, env: ASTTypeEnv): TIdent =
    lowerAST(ident, env, None)
  /* Some, but not all of the identifiers require function type
   * information to disambiguate them.
   * For ease of access, an overloaded function call is
   * provided
   */
  def lowerAST(ident: ASTIdent, env: ASTTypeEnv,
               typ: Option[ASTIdent]): TIdent =
    ident match {
      case ASTIdentVar(id) => TIdentVar(id)
      case ASTLongIdent(ids) => TIdentLongVar(ids.map{
        case ASTIdentVar(name) => name
        case id @ _ => throw new ICE("""ASTLongIdent contains non-ASTIdentVar
          type %s""".format(id.prettyPrint))
      })
      case ASTIdentTuple(subIdents) =>
        TIdentTuple(subIdents.map(lowerAST(_, env)))
      case ASTUnderscoreIdent() => TUnderscoreIdent()
      case ASTConsIdent() => TConsIdent()
      case ASTAppendIdent() => TAppendIdent()
      case ASTEmptyListIdent() => TEmptyListIdent()
      case ASTUnitIdent() => TUnitIdent()
      case ASTIntDivIdent() => TIntDivIdent()
      case ASTRealDivIdent() => TRealDivIdent()
      case ASTModIdent() => TIntModIdent()
      case ASTStringCatIdent() => TStringCatIdent()
      case ASTAndIdent() => TAnd()
      case ASTOrIdent() => TOr()
      case ASTUnOpNot() => TNot()
      case ASTUnOpPrint() => TPrint()
      /* This is  a bit of a mess. The idea is that some types
       * require specialization at this point in the tree. These
       * cases handle that. To enable the specialization, we
       * require that the 'typ' variable is set.
       *
       * This information is retrieved from the fun applicaton
       * type information in function applications
       */
      case _ => env(typ.get) match {
        case Some(ASTFunctionType(from, to)) =>
          ident match {
            case ASTPlusIdent() =>
              to match {
                case ASTIntType() => TIntPlusIdent()
                case ASTRealType() => TRealPlusIdent()
                case _ => unreachable
              }
            case ASTMinusIdent() =>
              to match {
                case ASTIntType() => TIntMinusIdent()
                case ASTRealType() => TRealMinusIdent()
                case _ => unreachable
              }
            case ASTTimesIdent() =>
              to match {
                case ASTIntType() => TIntMinusIdent()
                case ASTRealType() => TRealMinusIdent()
                case _ => unreachable
              }
            case ASTLEQIdent() =>
              from match {
                case ASTTupleType(List(ASTIntType(), _)) => TIntLEQIdent()
                case ASTTupleType(List(ASTRealType(), _)) => TRealLEQIdent()
                case ASTTupleType(List(ASTStringType(), _)) => TStringLEQIdent()
                case _ => unreachable
              }
            case ASTLTIdent() =>
              from match {
                case ASTTupleType(List(ASTIntType(), _)) => TIntLTIdent()
                case ASTTupleType(List(ASTRealType(), _)) => TRealLTIdent()
                case ASTTupleType(List(ASTStringType(), _)) => TStringLTIdent()
                case _ => unreachable
              }
            case ASTGEQIdent() =>
              from match {
                case ASTTupleType(List(ASTIntType(), _)) => TIntGEQIdent()
                case ASTTupleType(List(ASTRealType(), _)) => TRealGEQIdent()
                case ASTTupleType(List(ASTStringType(), _)) => TStringGEQIdent()
                case _ => unreachable
              }
            case ASTGTIdent() =>
              from match {
                case ASTTupleType(List(ASTIntType(), _)) => TIntGTIdent()
                case ASTTupleType(List(ASTRealType(), _)) => TRealGTIdent()
                case ASTTupleType(List(ASTStringType(), _)) => TStringGTIdent()
                case _ => unreachable
              }
            case ASTEqIdent() =>
              from match {
                case ASTIntType() => TIntEqualsIdent()
                case ASTRealType() => TRealEqualsIdent()
                case ASTStringType() => TStringEqualsIdent()
                case _ => TGenericEqualsIdent()
              }
            case ASTUnOpNegate() =>
              from match {
                case ASTIntType() => TNegInt()
                case ASTRealType() => TNegReal()
                case _ => unreachable
              }
            case _ => unreachable
          }
        case None => throw new ICE("""
            | Identifier %s should have been stripped out long before
            | this point. To lower into TIR, contextual awareness
            | is required.
            """.stripMargin.format(ident.prettyPrint))
      }
    }

  def lowerAST(typ: ASTType, env: ASTTypeEnv): TType = typ match {
    case ASTFunctionType(from, to) =>
      TFunctionType(lowerAST(from, env), lowerAST(to, env))
    case ASTTupleType(subTypes) =>
      TTupleType(subTypes map (lowerAST(_, env)))
    case ASTEqualityTypeVar(name) =>
      TEqualityTypeVar(name)
    case ASTUnconstrainedTypeVar(name) =>
      TUnconstrainedTypeVar(name)
    case ASTListType(subType) =>
      TListType(lowerAST(subType, env))
    case ASTIntType() => TIntType()
    case ASTRealType() => TRealType()
    case ASTBoolType() => TBoolType()
    case ASTStringType() => TStringType()
    case ASTCharType() => TCharType()
    case ASTUnitType() => TUnitType()
    case ASTDataTypeName(_) => ???
    // We 'expect' to hit this case if the typechecker failed
    // to specialize some values away from the internal types.
    case _ => throw new ICE("""Unexepected group type %s
      |left after typechecking.""".stripMargin.format(typ.prettyPrint))
  }

  def lowerAST(const: ASTConst, env: ASTTypeEnv): TConst = const match {
    /* These two cases are two of the few cases where this
     * lowering can fail.  If the integers are too big to fit
     * in normal sized ints, then we throw.  */
    case ASTConstInt(bigInt) => {
      val bigIntMax = BigInteger.valueOf(Integer.MAX_VALUE)
      val bigIntMin = BigInteger.valueOf(Integer.MIN_VALUE)

      if (bigInt.compareTo(bigIntMax) == 1
          || bigInt.compareTo(bigIntMin) == -1) {
        // Out of range
        throw new BadIntException(
          """Error: Constant %s is too large to fit in an int (32 bits
          |signed).""".stripMargin.format(bigInt.toString()))
      } else {
        // Otherwise we may convert as normal:
        TConstInt(bigInt.intValue())
      }
    }
    case ASTConstFloat(bigDecimal) =>
      // We convert floats out of range into infinities.
      TConstFloat(bigDecimal.doubleValue())
    case ASTConstString(string) => TConstString(string)
    case ASTConstChar(char) => TConstChar(char)
    case ASTConstTrue() => TConstTrue()
    case ASTConstFalse() => TConstFalse()
  }

  def lowerAST(pattern: ASTPat, env: ASTTypeEnv): TPat = pattern match {
    case ASTPatWildcard(typ) => TPatWildcard()
    case ASTPatVariable(name, typ) => name match {
      case ASTIdentVar(name) => TPatVariable(TIdentVar(name))
      case other => TPatIdentifier(lowerAST(other, env))
    }
    case ASTPatSeq(subseq, typ) => TPatSeq(subseq.map(lowerAST(_, env)))
    case ASTListPat(listpat, typ) => TListPat(listpat.map(lowerAST(_, env)))
    case ASTPatConst(const, typ) => TPatConst(lowerAST(const, env))
    case ASTPatCons(head, tail) =>
      TPatCons(lowerAST(head, env), lowerAST(tail, env))
  }

  def lowerAST(expr: ASTExp, env: ASTTypeEnv): TExp = expr match {
    case ASTExpConst(const) => TExpConst(lowerAST(const, env))
    case ASTExpIdent(ident) => TExpIdent(lowerAST(ident, env))
    case funApp @ ASTExpFunApp(fun, app) => {
      // This case has some nuance to it. If 'fun' is a function identifier,
      // then it might be specialized as part of this lowering.
      // To facilitate this, we need the type stored in this node.
      val funIdent = fun match {
        case ASTExpIdent(ident) =>
          TExpIdent(lowerAST(ident, env, Some(funApp.callType.get)))
        // If it is not an ident, then there will be some
        // later place at which the type can be recovered,
        // and so the type does not have to be passed on.
        case _ => lowerAST(fun, env)
      }

      val appIdent = lowerAST(app, env)
      val newType = lowerAST(funApp.callType.get, env)

      TExpFunApp(funIdent, appIdent, newType)
    }
    case app @ ASTExpInfixApp(operator, operand1, operand2) => {
      val newFunIdent = lowerAST(operator, env, Some(app.callType.get))
      val newOperand1 = lowerAST(operand1, env)
      val newOperand2 = lowerAST(operand2, env)

      val newIdent = lowerAST(app.callType.get, env)

      TExpFunApp(TExpIdent(newFunIdent),
                 TExpTuple(List(newOperand1, newOperand2)), newIdent)
    }
    case app @ ASTExpUnOpApply(operator, operand) => {
      val newFunIdent =
        TExpIdent(lowerAST(operator, env, Some(app.callType.get)))
      val newOperand = lowerAST(operand, env)
      val newIdent = lowerAST(app.callType.get, env)

      TExpFunApp(newFunIdent, newOperand, newIdent)
    }
    case ASTExpTuple(elems) => TExpTuple(elems.map(lowerAST(_, env)))
    case ASTExpList(elems) => TExpList(elems.map(lowerAST(_, env)))
    case let @ ASTExpLetIn(decs, exps) => {
      val loweredEnv = lowerEnv(let.typeEnv.get)
      val (loweredFuns, loweredVals)  = lowerAST(decs, let.typeEnv.get)
      val loweredExp = exps.map(lowerAST(_, let.typeEnv.get))

      // The design decision to treat a let-in in the AST
      // as a list of expressions rather than a single
      // expression was dubious one.
      //
      // Undo that decision here.
      val tExps = loweredExp match {
        case Nil => TExpIdent(TUnitIdent())
        case exp :: Nil => exp
        case exps => TExpSeq(exps)
      }

      TExpLetIn(loweredFuns ::: loweredVals, tExps, loweredEnv)
    }
    case ASTExpSeq(exps) =>
      TExpSeq(exps.map(lowerAST(_, env)))
    case ASTExpTyped(exp, typ) => lowerAST(exp, env)
    case ASTExpIfThenElse(cond, ifTrue, ifFalse) => {
      // For the sake of keeping the IR smaller, we implement this
      // as a case statement.
      val loweredEnv = lowerEnv(env)
      val ifTrueCase = TExpMatchRow(List(TPatConst(TConstTrue())),
                                    lowerAST(ifTrue, env),
                                    new TTypeEnv(Some(loweredEnv)))
      val ifFalseCase = TExpMatchRow(List(TPatConst(TConstFalse())),
                                     lowerAST(ifFalse, env),
                                     new TTypeEnv(Some(loweredEnv)))
      TExpCase(lowerAST(cond, env), List(ifTrueCase, ifFalseCase))
    }
    case ASTExpCase(exp, cases) =>
      TExpCase(lowerAST(exp, env), cases.map(lowerMatchRowAST(_, env)))
    case row @ ASTExpMatchRow(pattern, expr) =>
      TExpMatchRow(pattern.map(lowerAST(_, env)), lowerAST(expr, env),
                   lowerEnv(row.env.get))
    case expFn @ ASTExpFn(body) =>
      TExpFn(body.map(lowerMatchRowAST(_, env)),
             lowerAST(expFn.funType.get, env))
  }

  /* This is a separate function because match rows are not really
   * expressions. */
  def lowerMatchRowAST(row: ASTExpMatchRow, env: ASTTypeEnv): TExpMatchRow = {
    val loweredEnv = lowerEnv(row.env.get)

    TExpMatchRow(row.pat.map(lowerAST(_, row.env.get)),
                             lowerAST(row.exp, row.env.get), loweredEnv)
  }

  /* This is not as simple a task as one might imagine. The central
   * problem is that we do not want to lower environments more than
   * once, and that each environment has  a pointer
   * to a parent environment that may or may not have been lowered
   * already. To deal with this, we keep a hash table of already
   * lowered environments and pointers to those environments.
   *
   * Any environment entered will look for its environment in the table
   * before lowering itself.
   */
  val envMap =
    new HashMap[ASTTypeEnv, TTypeEnv]
  def lowerEnv(env: ASTTypeEnv): TTypeEnv = {
    // Check if this env is in the map. If so, return it.
    if (envMap.contains(env)) {
      return envMap(env)
    }

    // First convert the parent if possible:
    val newParent = env.parent map { case (env) => lowerEnv(env.getSelf) }
    val resEnv = new TTypeEnv(newParent)

    env.foreachInnermost{
      case (identifier, (resType, forallBoundVariables)) => {
        resEnv.add(lowerAST(identifier, env), lowerAST(resType, env),
          forallBoundVariables match {
            case Some(variables) =>
              Some(variables.map[TType]((_: Unit) => new TTypeSet(),
                                        (x: ASTType) => lowerAST(x, env)))
            case None => None
          })
      }
    }

    // Finally, set this new environment in the map so it is accessable
    envMap(env) = resEnv
    resEnv
  }

  def run(input: ASTProgram) = {
    try {
      lowerAST(input)
    } catch {
      case e: BadIntException => {
        println("Bad integer")
        println(e.getMessage())
        if (Shared.debug)
          e.printStackTrace()
        System.exit(1)
        unreachable
      }
    }
  }
}
