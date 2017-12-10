package lambda_lift

import change_names.ChangeIdentNames
import scala.collection.mutable.Map
import tir._

object UniqueifyVariablesWalk {
  def uniqueify(newVariables: Map[TNamedIdent,
                                  (List[TNamedIdent], List[TType])],
                function: TFun) = {
    val (freeValsNamesList, freeValsTypesList) = newVariables(function.name)
    assert(freeValsNamesList.length == freeValsTypesList.length)

    // We need to zip these with a list of TValClasses so that the
    // ChangeIdentNames pass knows what ident types to give these.
    val classes = List.fill(freeValsNamesList.length)(TValClass())

    // Now re-uniqueify the names of the arguments:
    // Create this in a new function rather than doing this elementwise
    // becaue the ChangeIdentNames function first creatse the new
    // names, and those (obviously) need to be consistent.
    // The environment we pass for the 'new' environment doesn't matter
    // because the walk will replace it with the one in the pattern
    // as a first step anyways.
    ChangeIdentNames.newNamesFor((freeValsNamesList, classes,
                                  freeValsTypesList).zipped.toList, function,
                                 new TTypeEnv(None))
  }

}
