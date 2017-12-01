package lambda_lift

import change_names.ChangeIdentNames
import scala.collection.mutable.Map
import tir._

object UniqueifyVariablesWalk {
  def uniqueify(newVariables: Map[TNamedIdent,
                                  (List[TNamedIdent], List[TType])],
                function: TFun) = {
    val (freeValsNamesList, freeValsTypesList) = newVariables(function.name)
    // Now re-uniqueify the names of the arguments:
    // Create this in a new function rather than doing this elementwise
    // becaue the ChangeIdentNames function first creatse the new
    // names, and those (obviously) need to be consistent.
    // The environment we pass for the 'new' environment doesn't matter
    // because the walk will replace it with the one in the pattern
    // as a first step anyways.
    ChangeIdentNames.newNamesFor(freeValsNamesList zip freeValsTypesList,
                                 function, new TTypeEnv(None))
  }

}
