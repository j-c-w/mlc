package t_typecheck

import exceptions._
import scala.collection.mutable.HashMap
import tir._
import tpass.TParentSetPass

/* This pass is designed to update the types in a pattern.
 * It is designed to walk between case statements.  Then it will be restarted.
 *
 * This needs a special case for each thing that can create an environment.
 * (i.e. if it is to be used before lambda lifting, it needs a case for
 * that).
 */
class SpecializeRowPatternWalk(renameMap: HashMap[TNamedIdent, TType])
    extends TParentSetPass[TType] {
  var seenMatchRow = false
  var matchEnv: Option[TTypeEnv] = None

  override def apply(targetType: TType, exp: TExp) = exp match {
    case TExpMatchRow(patterns, exp, env) => {
      assert(!seenMatchRow)
      assert(targetType.isInstanceOf[TFunctionType])

      // TODO -- Args needs to be a list of all the curried args.
      val (args, res) = targetType match {
        case TFunctionType(TTupleType(args), res) => (args, res)
        case _ => throw new UnreachableException()
      }

      matchEnv = Some(env)
      assert(patterns.length == args.length)
      (patterns zip args).foreach {
        case (pat, arg) => apply(arg, pat)
      }
      apply(res, exp)

      seenMatchRow = true
      None
    }
    // In this case, we need to start a new walk to deal with the new
    // environment.
    case caseStmt: TExpCase => {
      /// TODO -- Need to start a new walk here.
      ???
    }
    case other => super.apply(targetType, exp)
  }

  override def apply(targetType: TType, pat: TPat) = {
    assert(!seenMatchRow)
    pat match {
      case TPatWildcard() => // Nothing has to be done in this case
      case TPatVariable(identVar) => {
        // TODO -- Ideally, this should use updateId.  That would require
        // an implementation of the unifier in TType.
        matchEnv.get.updateIdNoValidate(identVar, targetType, None)
      }
      case TPatIdentifier(ident) => {
        ???
      }
      case TPatSeq(elems) => {
        targetType match {
          case TTupleType(typs) => {
            // TODO
            ???
          }
          case _ =>
            throw new ICE("Tuple typed with non tuple type %s"
              .format(targetType))
        }
      }
      case TListPat(elems) => {
        targetType match {
          case TListType(subTyp) =>
            elems.foreach(apply(subTyp, _))
          case _ =>
            throw new ICE("List typed with non list type %s"
              .format(targetType))
        }
      }
      case const: TPatConst =>
        // TODO -- It would be a good idea to assert that the const
        // is of the right type here.
      case TPatCons(hd, tl) => {
        targetType match {
          case TListType(subTyp) => {
            apply(subTyp, hd)
            apply(targetType, tl)
          }
          case _ =>
            throw new ICE("List typed witn non list type %s"
              .format(targetType))
        }
      }
    }
    None
  }

  override def apply(typ: TType, dec: TDec) = 
    // For the moment, no declarations are supported.  Should this walk be used
    // before lamnda lifting, it will need to walk through these cases, but can
    // probably ignore the patterns.
    throw new UnimplementedException("Specialization through declataions not" +
      " implemented yet.")
    
}
