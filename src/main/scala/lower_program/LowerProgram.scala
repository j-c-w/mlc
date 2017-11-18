package lower_program

import tir._
import toplev.Pass
import tpass.TPass
import typecheck.VariableGenerator

object LowerProgram extends Pass[TProgram, TJavaProgram]("lower_program") {
  def generateMain(vals: List[TVal], parentEnv: TTypeEnv) = {
    val rowEnv = new TTypeEnv(Some(parentEnv))
    val letEnv = new TTypeEnv(Some(rowEnv))

    val functionIdent = VariableGenerator.newTVariable()
    val valsExpression = TExpLetIn(vals, TExpIdent(TUnitIdent()), letEnv)

    val pattern = TExpMatchRow(List(TPatIdentifier(TUnitIdent())),
                               valsExpression, rowEnv)

    lowerFun(TFun(functionIdent, List(pattern)))
  }

  def lowerFun(fun: TFun) =
    TJavaFun(fun.name, lowerPatterns(fun.patterns))

  def lowerPatterns(patterns: List[TExpMatchRow]) = {
    var resultPatterns = List[TExpFunLetMatchRow]()
    for (row @ TExpMatchRow(pattern, _, env) <- patterns) {
      val removeLetsWalk = new RemoveLetsWalk()
      // Remove the lets from the expression
      val identifiers =
        (new GatherLetIdentifiersWalk(env)).apply(env, row.exp).toList
      val newOutterExp = removeLetsWalk.apply((), row.exp)

      newOutterExp.map(newExp => row.exp = newExp)

      // And now convert the pattern into a list of expressions
      // that uses the nth place argument to convert the
      // expressions.
      val (patternAssigns, patternTemps) =
        AssignmentGeneration.generateAssignsFromPattern(pattern).unzip

      row.exp = row.exp match {
        case TExpSeq(expressions) =>
          TExpSeq(patternAssigns.flatten ::: expressions)
        case _ => TExpSeq(patternAssigns.flatten :+ row.exp)
      }

      // Finally, construct the new patterns.  This involes
      // create the fun let with the row expression as computed above.
      //
      // We must ensure that all temp identifiers are in the identifier
      // list.
      val allIdentifiers =
        identifiers ::: patternTemps.flatten :::
        removeLetsWalk.accumulatedIdents.toList
      resultPatterns =
        TExpFunLetMatchRow(pattern, TExpFunLet(allIdentifiers, row.exp),
                           env) :: resultPatterns
    }

    // To avoid appending to the list for each pattern,
    // we reverse the list here.
    resultPatterns.reverse
  }

  def run(tree: TProgram) = {
    val mainFunction = generateMain(tree.vals, tree.typeEnv)

    tree.typeEnv.add(mainFunction.name,
                     TFunctionType(TUnitType(), TUnitType()), false)

    new TJavaProgram(tree.typeEnv, mainFunction, tree.funs.map(lowerFun(_)))
  }
}
