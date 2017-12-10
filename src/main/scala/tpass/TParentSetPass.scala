package tpass

import tir._
import exceptions.ICE

/*
 * This provides a walk of the tree that can optionally
 * 'set' the value of the parent to the returned value.
 *
 * This is provided in *addition* to TPass.
 *
 * TPass provides a walk that can be used to build results.
 *
 * This provides a walk that can set parts of the tree.
 *
 */

class TParentSetPass[T] {
  def getNew[T](old: T, possiblyNew: Option[T]): T = possiblyNew match {
    case Some(actuallyNew) => actuallyNew
    case None => old
  }

  def getNew[T](old: List[T], possiblyNew: List[Option[T]]): List[T] =
    getNew(old, possiblyNew, (x: T) => x)

  def getNew[T, U](old: List[T], possiblyNew: List[Option[U]],
                   conversion: U => T): List[T] = {
    // This are to detect bugs where it is natural
    // to add to the items in a list in a program before
    // this function returns.  In fact, because
    // of the way that this is designed, it will
    // be silently thrown away it if is done before.
    assert(old.length == possiblyNew.length)

    (old zip possiblyNew) map {
      case (old, Some(newElem)) => conversion(newElem)
      case (old, None) => old
    }
  }

  def apply(item: T, p: TTree): Unit = p match {
    case t: TConst => apply(item, t)
    case e: TExp => apply(item, e)
    case i: TIdent => apply(item, i)
    case p: TPat => apply(item, p)
    case t: TType => apply(item, t)
    case d: TDec => apply(item, d)
    case p: TProgram => apply(item, p)
    case j: TJavaProgram => apply(item, j)
  }

  def apply(item: T, p: TConst): Option[TConst] = p match {
    // These are all bottom cases.
    case const => None
  }

  def apply(item: T, p: TExp): Option[TExp] = {
    p match {
      case expConst @ TExpConst(const) => {
        apply(item, const) match {
        case None =>
        case Some(newChild) => expConst.const = newChild
      }
      }
      case expIdent @ TExpIdent(ident) => apply(item, ident) match {
        case None =>
        case Some(newIdent) => expIdent.ident = newIdent
      }
      case  app @ TExpFunApp(funname, application, callType) => {
        val funRes = apply(item, funname)
        val applicationRes = apply(item, application)

        val callTypeRes = apply(item, callType)

        app.funname = getNew(app.funname, funRes)
        app.application = getNew(app.application, applicationRes)
        app.callType =
          getNew(app.callType,
                 callTypeRes.map(_.asInstanceOf[TInternalIdentVar]))
      }
      case expTuple @ TExpTuple(tuple) => {
        val newTuples = tuple.map(apply(item, _))

        expTuple.elems = getNew(tuple, newTuples)
      }
      case expList @ TExpList(list) => {
        val newList = list.map(apply(item, _))

        expList.elems = getNew(list, newList)
      }
      case expSeq @ TExpSeq(seqs) => {
        val newSeqs = seqs.map(apply(item, _))

        expSeq.seq = getNew(seqs, newSeqs)
      }
      case letExpr @ TExpLetIn(decs, exp, env) => {
        val decsResults = decs.map(apply(item, _))
        val expResult = apply(item, exp)

        letExpr.decs = getNew(decs, decsResults)

        expResult match {
          case None =>
          case Some(expResult) => letExpr.exp = expResult
        }
      }
      case caseExpr @ TExpCase(exp, cases, typIdent) => {
        val expResult = apply(item, exp)
        val casesResults = cases.map(apply(item, _))
        val typIdentResults = apply(item, typIdent)

        caseExpr.cases = getNew(cases, casesResults,
                                (x: TExp) => x.asInstanceOf[TExpMatchRow])


        typIdentResults.map(typ =>
            caseExpr.applicationType = typ.asInstanceOf[TInternalIdentVar])
        expResult.map(expResult => caseExpr.exp = expResult)
      }
      case matchRow @ TExpMatchRow(pats, exp, env) => {
        val expResult = apply(item, exp)
        val patResults = pats.map(apply(item, _))

        matchRow.pat = getNew(pats, patResults)

        expResult.map(exp => matchRow.exp = exp)
      }
      case fnDec @ TExpFn(patterns, typ) => {
        val newPatterns = patterns.map(apply(item, _))
        val newTyp = apply(item, typ)

        newTyp.map(typ => fnDec.funType = typ.asInstanceOf[TInternalIdentVar])

        fnDec.patterns = getNew(patterns, newPatterns,
                                (x: TExp) => x.asInstanceOf[TExpMatchRow])
      }
      case assign @ TExpAssign(ident, expression) => {
        val newIdent = apply(item, ident)
        val newExpression = apply(item, expression)

        newIdent.map(ident =>
            assign.ident = ident.asInstanceOf[TNamedIdent])
        newExpression.map(exp => assign.expression = exp)
      }
      case head @ TExpListHead(list, typ) => {
        val newList = apply(item, list)
        val newTyp = apply(item, typ)

        head.list = getNew(list, newList)
        head.tyVar =
          getNew(typ, newTyp.map(_.asInstanceOf[TInternalIdentVar]))
      }
      case tail @ TExpListTail(list) => {
        val newList = apply(item, list)

        tail.list = getNew(list, newList)
      }
      case tupleExtract @ TExpTupleExtract(tuple, index, size, typ) => {
        val newTuple = apply(item, tuple)
        val newTyp = apply(item, typ)

        tupleExtract.tuple = getNew(tuple, newTuple)
        tupleExtract.tyVar =
          getNew(typ, newTyp.map(_.asInstanceOf[TInternalIdentVar]))
      }
      case listExtract @ TExpListExtract(list, index, typ) => {
        val newList = apply(item, list)
        val newTyp = apply(item, typ)

        listExtract.list = getNew(list, newList)
        listExtract.tyVar =
          getNew(typ, newTyp.map(_.asInstanceOf[TInternalIdentVar]))
      }
      case listLength @ TExpListLength(list) => {
        val newList = apply(item, list)

        listLength.list = getNew(listLength.list, newList)
      }
      case funLet @ TExpFunLet(valdecs, exp) => {
        val newVals = valdecs.map((x: TNamedIdent) => (x, apply(item, x)))
        val newExp = apply(item, exp)

        newVals.foreach {
          case (old, None) => // Do nothing
          case (old, Some(newIdent)) => {
            valdecs.remove(old)
            valdecs += newIdent.asInstanceOf[TNamedIdent]
          }
        }

        newExp.map(exp => funLet.exp = exp)
      }
      case ifStmt @ TExpIf(cond, ifTrue, ifFalse) => {
        val newCond = apply(item, cond)
        val newTrue = apply(item, ifTrue)
        val newFalse = apply(item, ifFalse)

        ifStmt.cond = getNew(ifStmt.cond, newCond)
        ifStmt.ifTrue = getNew(ifStmt.ifTrue, newTrue)
        ifStmt.ifFalse = getNew(ifStmt.ifFalse, newFalse)
      }
      case throwExp @ TExpThrow(throwable) => {
        val newThrowable = apply(item, throwable)

        throwExp.throwable =
          getNew(throwExp.throwable,
                 newThrowable).asInstanceOf[TIdentThrowable]
      }
    }
    None
  }

  def apply(item: T, p: TIdentClass): Option[TIdentClass] = None

  def apply(item: T, p: TIdent): Option[TIdent] = p match {
    case tuple @ TIdentTuple(subIdents) => {
      val newTypes = subIdents.map(apply(item, _))

      tuple.subTypes = getNew(subIdents, newTypes)
      None
    }
    case ident @ TIdentVar(_, identClass) => {
      val newClass = apply(item, identClass)

      ident.identClass = getNew(identClass, newClass)
      None
    }
    case ident @ TIdentLongVar(_, identClass) => {
      val newClass = apply(item, identClass)

      ident.identClass = getNew(identClass, newClass)
      None
    }
    case ident @ TTopLevelIdent(_, identClass) => {
      val newClass = apply(item, identClass)

      ident.identClass = getNew(identClass, newClass)
      None
    }
    // All other cases are base casses
    case other => None
  }

  def apply(item: T, p: TPat): Option[TPat] = {
    p match {
      case TPatWildcard() =>
      case patVar @ TPatVariable(name) => apply(item, name) match {
        case None =>
        case Some(result) => patVar.variable = result.asInstanceOf[TIdentVar]
      }
      case patIdent @ TPatIdentifier(ident) => apply(item, ident) match {
        case None =>
        case Some(result) => patIdent.identifier = result
      }
      case seqPat @ TPatSeq(seqs) => {
        val newSeqs = seqs.map(apply(item, _))

        seqPat.seq = getNew(seqs, newSeqs)
      }
      case listPat @ TListPat(items) => {
        val newItems = items.map(apply(item, _))

        listPat.listElems = (items zip newItems) map {
          case (old, Some(newItem)) => newItem
          case (old, None) => old
        }
      }
      case constPat @ TPatConst(const) =>
        apply(item, const) match {
          case None =>
          case Some(newConst) => constPat.const = newConst
        }
      case patCons @ TPatCons(head, tail) => {
        val headResult = apply(item, head)
        val tailResult = apply(item, tail)

        headResult match {
          case None =>
          case Some(newHead) => patCons.head = newHead
        }

        tailResult match {
          case None =>
          case Some(newTail) => patCons.tail = newTail
        }
      }
    }
    None
  }

  def apply(item: T, p: TType): Option[TType] = {
    p match {
      case funType @ TFunctionType(argType, resType) =>
        val argResult = apply(item, argType)
        val resResult = apply(item, resType)

        argResult match {
          case None =>
          case Some(newArg) => funType.argType = newArg
        }

        resResult match {
          case None =>
          case Some(newRes) => funType.resType = newRes
        }
      case tupleType @ TTupleType(subTypes) => {
        val newTypes = subTypes.map(apply(item, _))

        tupleType.subTypes = getNew(subTypes, newTypes)
      }
      case listType @ TListType(subType) => apply(item, subType) match {
        case None =>
        case Some(newType) => listType.subType = newType
      }
      case other => None
    }
    None
  }

  def apply(item: T, p: TDec): Option[TDec] = {
    p match {
      case valdec @ TVal(ident, exp) => {
        val newIdent = apply(item, ident)
        val newExp = apply(item, exp)

        newIdent.map(ident => valdec.ident = ident)
        newExp.map(exp => valdec.exp = exp)
      }
      case fundec @ TFun(ident, patterns) => {
        val newIdent = apply(item, ident)
        val newPatterns = patterns.map(apply(item, _))

        newIdent.map(ident => fundec.name = ident.asInstanceOf[TNamedIdent])
        fundec.patterns = getNew(patterns, newPatterns,
                                 (x: TExp) => x.asInstanceOf[TExpMatchRow])
      }
      case fundec @ TJavaFun(ident, curriedArgs, exp, env) => {
        val newIdent = apply(item, ident)
        val newExp = apply(item, exp)
        val curriedArgsResults = curriedArgs.map(apply(item, _))

        fundec.name =
          getNew(ident, newIdent.map(_.asInstanceOf[TTopLevelIdent]))
        fundec.exp = getNew(exp, newExp.map(_.asInstanceOf[TExpFunLet]))
        fundec.curriedArgs =
          getNew(curriedArgs,
                 curriedArgsResults,
                 (x: TIdent) => (x.asInstanceOf[TInternalIdentVar]))
      }
    }
    None
  }

  def apply(item: T, p: TProgram): Unit = {
    val funsRes = p.funs.map(apply(item, _))
    val valsRes = p.vals.map(apply(item, _))

    p.funs = getNew(p.funs, funsRes, (x: TDec) => x.asInstanceOf[TFun])

    p.vals = getNew(p.vals, valsRes, (x: TDec) => x.asInstanceOf[TVal])
  }

  def apply(item: T, p: TJavaProgram): Unit = {
    val mainRes = apply(item, p.main)
    val funRes = p.functions.map(apply(item, _))
    val identsResults = p.topLevelVariables.map(x => (x, apply(item, x)))

    mainRes.map(main => p.main = main.asInstanceOf[TJavaFun])
    p.functions = getNew(p.functions, funRes,
                         (x: TDec) => x.asInstanceOf[TJavaFun])
    identsResults.foreach {
      case (old, None) => // Do nothing
      case (old, Some(newIdent)) => {
        p.topLevelVariables.remove(old)
        p.topLevelVariables += newIdent.asInstanceOf[TTopLevelIdent]
      }
    }
  }
}
