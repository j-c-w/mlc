package ast_change_names

import change_names.FunctionNameGenerator
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

      ASTValBind(newIdentifiers.asInstanceOf[ASTIdentTuple], newExpression)
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
          // Types need not be renamed.
          val newExpression = changeNames(newNameEnv, expression)

          (newIdents, newPattern, types, newExpression)
        }
      })
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
      ASTExpTyped(changeNames(map, exp), typ)
    case ASTExpMatchRow(pats, exp) => {
      val newMap = new ASTNameEnvironment(map)

      val newPats = pats.map(changeNamesInsert(newMap, _))
      val newExp = changeNames(newMap, exp)

      ASTExpMatchRow(newPats, newExp)
    }
    case ASTExpFn(body) =>
      ASTExpFn(body.map(changeNamesMatchRow(map, _)))
    case ASTExpIfThenElse(cond, t, f) =>
      ASTExpIfThenElse(changeNames(map, cond), changeNames(map, t),
                       changeNames(map, f))
    case ASTExpCase(exp, cases) =>
      ASTExpCase(changeNames(map, exp), cases.map(changeNamesMatchRow(map, _)))
  }

  /* If this compiler were to be expanded to a larger subset, this
   * pass would have to be rewritten. */
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
        ASTPatVariable(changeNamesInsertNoDuplicate(map, variable), typ)
      case ASTPatSeq(pats, typs) =>
        ASTPatSeq(pats.map(changeNamesInsert(map, _)), typs)
      case ASTListPat(list, typs) =>
        ASTListPat(list.map(changeNamesInsert(map, _)), typs)
      case ASTPatCons(head, tail) =>
        ASTPatCons(changeNamesInsert(map, head), changeNamesInsert(map, tail))
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
