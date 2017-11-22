package tpass

import tir._

/*
 * This is an interface for TreeIR passes.
 *
 * An object/class implementing this should then
 * be able to call TIR.walk(this) and have
 * the function defined called on every node.
 *
 * This abstracts away a large amount of the boilerplate
 * descent code.
 *
 * Each should return true to continue walking down the tree
 * and false to stop walking down the tree.
 */

trait TPass[T, U] {
  def default: U
  def combine(recursiveResult: U, otherResult: U): U

  def apply(item: T, p: TConst): U = p match {
    // These are all bottom cases.
    case const => default
  }

  def apply(item: T, p: TExp): U = p match {
    case TExpConst(const) => apply(item, const)
    case TExpIdent(ident) => apply(item, ident)
    case  TExpFunApp(funname, application, callType) => {
      val funRes = apply(item, funname)
      val applicationRes = apply(item, application)

      val callTypeRes = apply(item, callType)

      combine(combine(funRes, applicationRes), callTypeRes)
    }
    case TExpTuple(tuple) => {
      combineList(tuple.map(apply(item, _)))
    }
    case TExpList(list) => {
      combineList(list.map(apply(item, _)))
    }
    case TExpSeq(seqs) => {
      combineList(seqs.map(apply(item, _)))
    }
    case TExpLetIn(decs, exp, env) => {
      val decsResults = decs.map(apply(item, _))
      val expResult = apply(item, exp)

      combine(expResult, combineList(decsResults))
    }
    case TExpCase(exp, cases, typ) => {
      val expResult = apply(item, exp)
      val casesResults = cases.map(apply(item, _))
      val typResult = apply(item, typ)

      combine(combine(expResult, combineList(casesResults)), typResult)
    }
    case TExpMatchRow(pats, exp, env) => {
      val expResult = apply(item, exp)
      val patResults = pats.map(apply(item, _))

      combine(expResult, combineList(patResults))
    }
    case TExpFn(patterns, typ) =>
      combine(combineList(patterns.map(apply(item, _))),
              apply(item, typ))
    case TExpAssign(ident, expression) =>
      combine(apply(item, ident), apply(item, expression))
    case TExpListHead(list) =>
      apply(item, list)
    case TExpListTail(list) =>
      apply(item, list)
    case TExpTupleExtract(tuple, index) =>
      apply(item, tuple)
    case TExpListExtract(list, index) =>
      apply(item, list)
    case TExpListLength(list) =>
      apply(item, list)
    case TExpFunLet(idents, expression) =>
      combine(combineList(idents.map(apply(item, _))),
              apply(item, expression))
    case TExpIf(cond, ifTrue, ifFalse) =>
      combine(combine(apply(item, cond),
                      apply(item, ifTrue)),
              apply(item, ifFalse))
    case TExpThrow(throwable) =>
      apply(item, throwable)
  }

  def apply(item: T, p: TIdent): U = p match {
    case TIdentTuple(subTypes) => combineList(subTypes.map(apply(item, _)))
    // All other cases are base casses
    case other => default
  }

  def apply(item: T, p: TPat): U = p match {
    case TPatWildcard() => default
    case TPatVariable(name) => apply(item, name)
    case TPatIdentifier(ident) => apply(item, ident)
    case TPatSeq(seqs) => combineList(seqs.map(apply(item, _)))
    case TListPat(items) => combineList(items.map(apply(item, _)))
    case TPatConst(const) => apply(item, const)
    case TPatCons(head, tail) => combine(apply(item, head), apply(item, tail))
  }

  def apply(item: T, p: TType): U = p match {
    case TFunctionType(argType, resType) =>
      combine(apply(item, argType), apply(item, resType))
    case TTupleType(subTypes) =>
      combineList(subTypes.map(apply(item, _)))
    case TListType(subType) => apply(item, subType)
    case other => default
  }

  def apply(item: T, p: TDec): U = p match {
    case TVal(ident, exp) => combine(apply(item, ident), apply(item, exp))
    case TFun(ident, patterns) => {
      val identRes = apply(item, ident)
      val casesRes = patterns.map(apply(item, _))

      combine(combineList(casesRes), identRes)
    }
    case TJavaFun(ident, exp, env) => {
      val identRes = apply(item, ident)
      val casesRes = apply(item, exp)

      combine(casesRes, identRes)
    }
  }

  def apply(item: T, p: TProgram): U = {
    val funsRes = p.funs.map(apply(item, _))
    val valsRes = p.vals.map(apply(item, _))

    combine(combineList(funsRes), combineList(valsRes))
  }

  def apply(item: T, p: TJavaProgram): U = {
    val mainRes = apply(item, p.main)
    val funsRes = p.functions.map(apply(item, _))

    combine(combineList(funsRes), mainRes)
  }

  def combineList(list: List[U]): U = 
    if (list.length > 0)
      list.tail.foldLeft(list.head) {
        case (buildUp, head) => combine(buildUp, head)
      }
    else {
      default
    }
}
