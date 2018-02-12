package ast_change_names

import exceptions._
import frontend._
import toplev.Pass
import toplev.Shared

/* This pass takes the input and restructures the names so that
 * all variable names are unique.  */
object ASTChangeNames
    extends Pass[ASTProgram, ASTProgram]("ast_change_names") {

  def changeNames(map: ASTNameEnvironment,
                  dec: ASTDeclaration): ASTDeclaration = dec match {
    case ASTValBind(ident, expression) => {
      // Do the expression first.
      val newExpression = changeNames(map, expression)
      val newIdentifiers = changeNamesInsert(map, ident)

      ASTValBind(newIdentifiers.asInstanceOf[ASTIdent], newExpression)
    }
    case ASTFunBind(cases) => {
      // The identifier should be inserted in the parent.
      assert(cases.length >= 1)

      // Insert the new function name
      changeNamesInsert(map, cases(0)._1)

      ASTFunBind(cases.map {
        case (ident, patterns, types, expression) => {
          // All subsequent names may be simply 'changed'
          val newIdents = changeNames(map, ident)

          val newNameEnv = new ASTNameEnvironment(map)
          val newPattern = changeNamesInsert(newNameEnv, patterns)
          val newExpression = changeNames(newNameEnv, expression)
          val newTypes = types.map(changeNames(map, _))

          (newIdents, newPattern, newTypes, newExpression)
        }
      })
    }
    case ASTExceptionBind(name, typs) => {
      changeNamesInsert(map, name)
      // Declaration of this exception is left unchanged.  Exceptions
      // may not appear in the types of an exception.
      ASTExceptionBind(changeNames(map, name), typs.map(changeNames(map, _)))
    }
    case ASTDataType(_, _) =>
      throw new UnimplementedException("Datatypes are not implemented.")
  }

  def changeNames(map: ASTNameEnvironment, exp: ASTExp): ASTExp = exp match {
    case ASTExpIdent(name) => name match {
      case ASTIdentVar(name) => ASTExpIdent(ASTIdentVar(map(name)))
      case other => ASTExpIdent(other)
    }
    case const @ ASTExpConst(_) => const
    case ASTExpFunApp(function, application) =>
      ASTExpFunApp(changeNames(map, function), changeNames(map, application))
    case ASTExpInfixApp(operator, operand1, operand2) =>
      ASTExpInfixApp(changeNamesInfixIdent(map, operator),
                     changeNames(map, operand1),
                     changeNames(map, operand2))
    case ASTExpUnOpApply(operator, operand) =>
      ASTExpUnOpApply(changeNamesUnOp(map, operator),
                      changeNames(map, operand))
    case ASTExpTuple(items) =>
      ASTExpTuple(items.map(changeNames(map, _)))
    case ASTExpList(items) =>
      ASTExpList(items.map(changeNames(map, _)))
    case ASTExpLetIn(decs, exps) => {
      val newMap = new ASTNameEnvironment(map)

      // Type the decs the the expression
      val newDecs = decs.map(changeNames(newMap, _))
      val newExps = exps.map(changeNames(newMap, _))

      ASTExpLetIn(newDecs, newExps)
    }
    case ASTExpSeq(seqs) =>
      ASTExpSeq(seqs.map(changeNames(map, _)))
    case ASTExpTyped(exp, typ) =>
      ASTExpTyped(changeNames(map, exp), changeNames(map, typ))
    case ASTExpMatchRow(pats, exp) => {
      val newMap = new ASTNameEnvironment(map)

      val newPats = pats.map(changeNamesInsert(newMap, _))
      val newExp = changeNames(newMap, exp)

      ASTExpMatchRow(newPats, newExp)
    }
    case ASTExpRaise(exception) =>
      ASTExpRaise(changeNames(map, exception))
    case ASTExpHandle(exp, cases) =>
      ASTExpHandle(changeNames(map, exp),
                   cases.map(changeNamesMatchRow(map, _)))
    case ASTExpFn(body) =>
      ASTExpFn(body.map(changeNamesMatchRow(map, _)))
    case ASTExpIfThenElse(cond, t, f) =>
      ASTExpIfThenElse(changeNames(map, cond), changeNames(map, t),
                       changeNames(map, f))
    case ASTExpCase(exp, cases) =>
      ASTExpCase(changeNames(map, exp), cases.map(changeNamesMatchRow(map, _)))
  }

  /* If this compiler were to be expanded to a larger subset including
   * infix declarations, this pass would have to be rewritten. */
  def changeNamesUnOp(map: ASTNameEnvironment, ident: ASTUnOp) =
    ident

  def changeNamesInfixIdent(map: ASTNameEnvironment, ident: ASTInfixIdent) =
    ident

  def changeNamesMatchRow(map: ASTNameEnvironment, row: ASTExpMatchRow) =
    changeNames(map, row).asInstanceOf[ASTExpMatchRow]

  def changeNames(map: ASTNameEnvironment, ident: ASTIdent): ASTIdent =
    ident match {
      case ASTIdentVar(name) => ASTIdentVar(map(name))
      case ASTIdentTuple(list) =>
        ASTIdentTuple(list.map(changeNamesInsert(map, _)))
      case other => other
    }

  def changeNamesInsert(map: ASTNameEnvironment, ident: ASTIdent): ASTIdent =
    ident match {
      case ASTIdentVar(name) => {
          val newName = FunctionNameGenerator.newIdentName(name)
          map.add(name, newName)

          ASTIdentVar(newName)
        }
      case ASTIdentTuple(list) =>
        ASTIdentTuple(list.map(changeNamesInsert(map, _)))
      case other => other
    }

  def changeNamesInsertNoDuplicate(map: ASTNameEnvironment,
                                   ident: ASTIdent): ASTIdent = ident match {
      case ASTIdentVar(name) => {
        val newName = FunctionNameGenerator.newIdentName(name)
        map.addIfNew(name, newName)

        ASTIdentVar(newName)
      }
      case ASTIdentTuple(list) =>
        ASTIdentTuple(list.map(changeNamesInsert(map, _)))
      case other => other
  }

  def changeNamesInsert(map: ASTNameEnvironment,
                        pattern: List[ASTPat]): List[ASTPat] =
    pattern.map(changeNamesInsert(map, _))

  /* In this method, we must be careful to account for doubly named
   * elements. */
  def changeNamesInsert(map: ASTNameEnvironment, pattern: ASTPat): ASTPat =
    pattern match {
      case ASTPatVariable(variable, typ) =>
        ASTPatVariable(changeNamesInsertNoDuplicate(map, variable),
                       changeNames(map, typ))
      case ASTPatSeq(pats, typs) =>
        ASTPatSeq(pats.map(changeNamesInsert(map, _)), changeNames(map, typs))
      case ASTListPat(list, typs) =>
        ASTListPat(list.map(changeNamesInsert(map, _)), changeNames(map, typs))
      case ASTPatConstructor(name, args, typs) => {
        val newName = changeNames(map, name)

        ASTPatConstructor(newName,
                          args.map(changeNamesInsert(map, _)
                            .asInstanceOf[ASTPatSeq]),
                          changeNames(map, typs))
      }
      case ASTPatCons(head, tail, typs) =>
        ASTPatCons(changeNamesInsert(map, head),
                   changeNamesInsert(map, tail),
                   changeNames(map, typs))
      case other => other
    }

  def changeNames(map: ASTNameEnvironment,
                        typList: List[ASTType]): List[ASTType] =
    typList.map(changeNames(map, _))

  def changeNames(map: ASTNameEnvironment, typ: ASTType): ASTType =
    typ match {
      case ASTDataTypeName(name) =>
        ASTDataTypeName(changeNames(map, name))
      case ASTFunctionType(from, to) =>
        ASTFunctionType(changeNames(map, from),
                        changeNames(map, to))
      case ASTTupleType(elems) =>
        ASTTupleType(elems.map(changeNames(map, _)))
      case ASTListType(subTyp) =>
        ASTListType(changeNames(map, subTyp))
      case other => other
    }

  def run(tree: ASTProgram): ASTProgram = {
    val substitutionMap = new ASTNameEnvironment()

    try {
      new ASTProgram(tree.decs.map(changeNames(substitutionMap, _)))
    } catch {
      case e: UnrecognizedIdentifierError => {
        println("Unrecognized identifier")
        println(e.getMessage())
        if (Shared.debug)
          e.printStackTrace()
        System.exit(1)
        unreachable
      }
      case e: UnimplementedException => {
        println("Datatypes not implemented")
        println(e.getMessage())
        if (Shared.debug)
          e.printStackTrace()
        System.exit(1)
        unreachable
      }
      case e: BadPatternException => {
        println("Bad pattern error")
        println(e.getMessage())
        if (Shared.debug)
          e.printStackTrace()
        System.exit(1)
        unreachable
      }
    }
  }
}
