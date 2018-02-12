package lower_program

import exceptions.ICE
import generators.VariableGenerator
import scala.collection.mutable.{HashSet,Set}
import tir._
import toplev.Pass
import tpass.TPass

object LowerProgram extends Pass[TProgram, TJavaProgram]("lower_program") {
  def generateMain(vals: List[TVal], parentEnv: TTypeEnv) = {
    val rowEnv = new TTypeEnv(Some(parentEnv))
    val letEnv = new TTypeEnv(Some(rowEnv))

    val valNames = new HashSet[TTopLevelIdent]()
    vals.foreach {
      case TVal(ident @ TTopLevelIdent(_, _), _) => valNames.add(ident)
      case TVal(ident @ TIdentTuple(_), _) =>
        ident.getDeclaredIdents.foreach((x: TNamedIdent) =>
          valNames.add(x.asInstanceOf[TTopLevelIdent]))
      case TVal(TUnderscoreIdent(), _) =>
      case other => throw new ICE("""Top level val dec without
        |a TTopLevelIdent identifier type""".stripMargin)
    }

    val functionIdent = VariableGenerator.newTTopLevelVariable(TFunClass())

    // Also need to add this new function to the type environment.
    val functionType = TFunctionType(TUnitType(), TUnitType())
    parentEnv.add(functionIdent, functionType, false)

    val valsExpression = TExpLetIn(vals, TExpIdent(TUnitIdent()), letEnv)

    val pattern = TExpMatchRow(List(TPatIdentifier(TUnitIdent())),
                               valsExpression, rowEnv)
    (lowerFun(TFun(functionIdent, List(pattern)), parentEnv), valNames)
  }

  def lowerFun(fun: TFun, parent: TTypeEnv) = {
    // Get the pattern out of the function to get the type of it out:
    // This has to be done here, before we throw away the structural
    // information to avoid running into issues assuming that
    // f: 'a -> 'b -> 'c
    // is in fact of the form fun f x y = z (could be
    // fun f x = fn y => z).
    def getTypeFrom(patternLength: Int, typ: TType): List[TInternalIdentVar] =
      (patternLength, typ) match {
        case (0, other) => List()
        case (n, TFunctionType(arg, res)) => {
          val variable = VariableGenerator.newTInternalVariable()
          parent.addTopLevel(variable, arg, false)

          variable :: getTypeFrom(patternLength - 1, res)
        }
        case other => throw new ICE("""Curried function with %s
          |more function types in the argument, but no more function types in
          |the type!""".stripMargin.format(patternLength))
      }

    val curriedApps =
      getTypeFrom(fun.patterns(0).pat.length, parent.getOrFail(fun.name))
    val topLevelName = fun.name.asInstanceOf[TTopLevelIdent]

    val (representativeExp, newEnv) =
      lowerPatterns(topLevelName, fun.patterns, parent)
    TJavaFun(topLevelName, curriedApps,
             representativeExp, newEnv)
  }

  def lowerPatterns(name: TTopLevelIdent, patterns: List[TExpMatchRow],
                    parentEnv: TTypeEnv) = {
    // The first pattern is just to throw. This is inserted
    // as the last 'else' case.
    var resultPattern: TExp = TExpRaise(TExpIdent(TIdentMatchError()))
    val resultEnv = new TTypeEnv(Some(parentEnv))
    var allIdentifiers: Set[TNamedIdent] = new HashSet[TNamedIdent]()

    val mergeEnvsWalk = new MergeTypeEnvsWalk(resultEnv)

    // This stores the argument types as a list.  So, if the type
    // is a -> b -> c, this is [a, b]
    val argumentTypesList = functionArgumentTypeList(parentEnv.getOrFail(name))
    
    // We use the reversed pattern so that the innermost nested else
    // case is covered first.
    for (row @ TExpMatchRow(pattern, _, env) <- patterns.reverse) {
      // Ensure that all the identifiers from the expression are in
      // the resultEnv
      mergeEnvsWalk.apply(env, row)

      // Remove the lets from the expression.
      val identifiers =
        (new GatherLetIdentifiersWalk(resultEnv)).apply(resultEnv,
                                                        row.exp).toList
      val removeLetsWalk = new RemoveLetsWalk(resultEnv)
      val newOutterExp = removeLetsWalk.apply((), row.exp)

      newOutterExp.map(newExp => row.exp = newExp)

      // Since these are functions, we generate the function arguments
      // so that the pattern matching knows what to extract from:
      val arguments: List[TIdent] =
        (0 until pattern.length).map(TArgumentNode(name, _)).toList

      // We used to check that these arguments were the same length.
      // However, there are problems with doing this for functions
      // like this:
      //    fun f x = fn y => 1
      // 
      // (Which has the same type as fun f x y = 1)
      //
      // And so the first would fail an assertion because the arguments
      // were shorter than the types list (which was extracted purely from
      // the types).
      // Therefore, we silently chop off any additional elements in the list.
      // These arguments must be added to the type environment.
      (arguments zip argumentTypesList).foreach {
        case (arg, argType) => resultEnv.add(arg, argType, false)
      }

      // And now convert the pattern into a list of expressions
      // that uses the nth place argument to convert the
      // expressions.
      val (patternAssigns, patternTemps) =
        AssignmentGeneration.
          generateAssignsFromPattern(pattern, arguments, resultEnv)

      // Finally, construct the new patterns.  This involes
      // create the fun let with the row expression as computed above.
      //
      // We must ensure that all temp identifiers are in the identifier
      // list.
      allIdentifiers ++= identifiers 
      allIdentifiers ++= patternTemps.flatten
      allIdentifiers ++= removeLetsWalk.accumulatedIdents.toList

      resultPattern = TExpIf(patternAssigns, row.exp, resultPattern)
    }

    // We put the result pattern in a Let binding since later
    // passes require that all variables that require stack
    // space are bound within let expressions.
    (TExpFunLet(allIdentifiers, resultPattern), resultEnv)
  }

  /* Given some (possibly curried) function type, this returns a list
   * of all the arguments of that function.  Note that this DOES NOT INCLUDE
   * the result type of the function.  */
  def functionArgumentTypeList(typ: TType): List[TType] = typ match {
    case TFunctionType(arg, res) => arg :: functionArgumentTypeList(res)
    // Do not include the result type
    case other => List[TType]()
  }

  def run(tree: TProgram) = {
    val (mainFunction, valDecs) = generateMain(tree.vals, tree.typeEnv)

    tree.typeEnv.add(mainFunction.name,
                     TFunctionType(TUnitType(), TUnitType()), false)

    new TJavaProgram(tree.typeEnv, mainFunction, valDecs,
                     tree.dataTypeDecs,
                     tree.funs.map(lowerFun(_, tree.typeEnv)))
  }
}
