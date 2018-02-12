package lift_declarations

import change_names.ChangeIdentNames
import exceptions.UnreachableException
import scala.collection.mutable.HashMap
import tir._
import toplev.Pass

/* This walk lifts nested declarations to the top level.
 *
 * For example, if we have:
 *
 *  let
 *    datatype f = x of string
 *  in
 *    ...
 *  end
 *
 * This lifts 'f' to the top level.
 */

object LiftDecs extends Pass[TProgram, TProgram]("lift_decs") {
  def run(program: TProgram) = {
    val walk = new LiftDecsWalk()
    walk.apply(program.typeEnv, program)
    program.dataTypeDecs = program.dataTypeDecs ::: walk.newTopDecs

    // We need to go through the program and replace all references
    // to the top decs with TTopLevelIdents.  Start by creating the mapping.
    val mapping = new HashMap[TNamedIdent, (TNamedIdent, TType)]()
    walk.newTopDecs.foreach {
      case dec @ TDataTypeDec(ident @ TIdentVar(nameString, identClass),
                              _, _) => {
        dec.name = TTopLevelIdent(nameString, identClass)
        mapping(ident) = (TTopLevelIdent(nameString, identClass),
                          program.typeEnv.getOrFail(ident))
      }
      case _ => throw new UnreachableException()
    }

    ChangeIdentNames.newNamesFor(mapping, program, program.typeEnv)

    program
  }
}
