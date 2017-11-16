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
  def apply(item: T, p: TConst): Option[TConst] = p match {
    // These are all bottom cases.
    case const => None
  }

  def apply(item: T, p: TExp): Option[TExp] = {
    p match {
      case expConst @ TExpConst(const) => apply(item, const) match {
        case None =>
        case Some(newChild) => expConst.const = newChild
      }
      case expIdent @ TExpIdent(ident) => apply(item, ident) match {
        case None =>
        case Some(newIdent) => expIdent.ident = newIdent
      }
      case  app @ TExpFunApp(funname, application, callType) => {
        val funRes = apply(item, funname)
        val applicationRes = apply(item, application)

        val callTypeRes = apply(item, callType)

        funRes match {
          case None =>
          case Some(newFunRes) => app.funname = newFunRes
        }

        applicationRes match {
          case None =>
          case Some(newApplication) => app.application = newApplication
        }

        callTypeRes match {
          case None =>
          case Some(newCallType) => app.callType = newCallType
        }
      }
      case expTuple @ TExpTuple(tuple) => {
        val newTuples = tuple.map(apply(item, _))

        expTuple.elems = (tuple zip newTuples) map {
          case (old, Some(newElem)) => newElem
          case (old, None) => old
        }
      }
      case expList @ TExpList(list) => {
        val newList = list.map(apply(item, _))

        expList.elems = (list zip newList) map {
          case (old, Some(newItem)) => newItem
          case (old, None) => old
        }
      }
      case expSeq @ TExpSeq(seqs) => {
        val newSeqs = seqs.map(apply(item, _))

        expSeq.seq = (seqs zip newSeqs) map {
          case (old, Some(newElem)) => newElem
          case (old, None) => old
        }
      }
      case letExpr @ TExpLetIn(decs, exp, env) => {
        val decsResults = decs.map(apply(item, _))
        val expResult = apply(item, exp)

        letExpr.decs = (decs zip decsResults) map {
          case (old, Some(newDec)) => newDec
          case (old, None) => old
        }

        expResult match {
          case None =>
          case Some(expResult) => letExpr.exp = expResult
        }
      }
      case caseExpr @ TExpCase(exp, cases) => {
        val expResult = apply(item, exp)
        val casesResults = cases.map(apply(item, _))

        caseExpr.cases = (cases zip casesResults) map {
          case (old, Some(newElem)) => newElem.asInstanceOf[TExpMatchRow]
          case (old, None) => old
        }

        expResult match {
          case None =>
          case Some(expResult) => caseExpr.exp = expResult
        }
      }
      case matchRow @ TExpMatchRow(pats, exp, env) => {
        val expResult = apply(item, exp)
        val patResults = pats.map(apply(item, _))

        matchRow.pat = (pats zip patResults) map {
          case (old, Some(newElem)) => newElem
          case (old, None) => old
        }

        expResult.map(exp => matchRow.exp = exp)
      }
      case fnDec @ TExpFn(patterns, typ) => {
        val newPatterns = patterns.map(apply(item, _))
        val newTyp = apply(item, typ)

        newTyp.map(typ => fnDec.funType = typ)

        fnDec.patterns = (patterns zip newPatterns) map {
          case (old, Some(newPat)) => newPat.asInstanceOf[TExpMatchRow]
          case (old, None) => old
        }
      }
      case assign @ TExpAssign(ident, expression) => {
        val newIdent = apply(item, ident)
        val newExpression = apply(item, expression)

        newIdent.map(ident => assign.ident = ident.asInstanceOf[TIdentVar])
        newExpression.map(exp => assign.expression = exp)
      }
      case funLet @ TExpFunLet(valdecs, exp) => {
        val newVals = valdecs.map(apply(item, _))
        val newExp = apply(item, exp)

        funLet.valdecs = (valdecs zip newVals) map {
          case (old, Some(newVal)) => newVal.asInstanceOf[TIdentVar]
          case (old, _) => old
        }
        newExp.map(exp => funLet.exp = exp)
      }
      case matchRow @ TExpFunLetMatchRow(pattern, exp, env) => {
        val expResult = apply(item, exp)
        val patResults = pattern.map(apply(item, _))

        matchRow.pat = (pattern zip patResults) map {
          case (old, Some(newElem)) => newElem
          case (old, None) => old
        }

        expResult.map(exp => matchRow.exp = exp.asInstanceOf[TExpFunLet])
      }
    }
    None
  }

  def apply(item: T, p: TIdent): Option[TIdent] = p match {
    case tuple @ TIdentTuple(subIdents) => {
      val newTypes = subIdents.map(apply(item, _))

      tuple.subTypes = (subIdents zip newTypes) map {
        case (old, Some(newType)) => newType
        case (old, None) => old
      }

      None
    }
    case ident @ TIdentVar(name) => None
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

        seqPat.seq = (seqs zip newSeqs) map {
          case (old, Some(newSeq)) => newSeq
          case (old, None) => old
        }
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

        tupleType.subTypes = (subTypes zip newTypes) map {
          case (old, Some(newType)) => newType
          case (old, None) => old
        }
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

        newIdent.map(ident => fundec.name = ident.asInstanceOf[TIdentVar])
        fundec.patterns = (patterns zip newPatterns) map {
          case (old, Some(newMatchRow)) =>
            newMatchRow.asInstanceOf[TExpMatchRow]
          case (old, None) => old
        }
      }
      case fundec @ TJavaFun(ident, patterns) => {
        val newIdent = apply(item, ident)
        val newPatterns = patterns.map(apply(item, _))

        newIdent.map(ident => fundec.name = ident.asInstanceOf[TIdentVar])
        fundec.cases = (patterns zip newPatterns) map {
          case (old, Some(newMatchRow)) =>
            newMatchRow.asInstanceOf[TExpFunLetMatchRow]
          case (old, None) => old
        }
      }
    }
    None
  }

  def apply(item: T, p: TProgram): Unit = {
    val funsRes = p.funs.map(apply(item, _))
    val valsRes = p.vals.map(apply(item, _))

    // These are to detect bugs where it is natural
    // to add to the functions in a program before
    // this function returns.  In fact, because
    // of the way that this is designed, it will
    // be silently thrown away it if is done before.
    assert(p.funs.length == funsRes.length)
    assert(p.vals.length == valsRes.length)

    p.funs = (p.funs zip funsRes) map {
      case (old, Some(newFun)) => newFun.asInstanceOf[TFun]
      case (old, None) => old
    }

    p.vals = (p.vals zip valsRes) map {
      case (old, Some(newVal)) => newVal.asInstanceOf[TVal]
      case (old, None) => old
    }
  }

  def apply(item: T, p: TJavaProgram): Unit = {
    val mainRes = apply(item, p.main)
    val funRes = p.functions.map(apply(item, _))

    assert(p.functions.length == funRes.length)
    mainRes.map(main => p.main = main.asInstanceOf[TJavaFun])
    p.functions = (p.functions zip funRes) map {
      case (old, Some(newFun)) => newFun.asInstanceOf[TJavaFun]
      case (old, None) => old
    }
  }
}
