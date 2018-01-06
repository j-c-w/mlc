package t_inline

import scala.collection.mutable.Set
import toplev.GenericPrintable
import tir._

sealed trait InlineReason extends GenericPrintable

case class Inline(val inliningCost: Int, val thresh: Int,
                  val funName: TNamedIdent,
                  // This must store all the curried arguments.  One
                  // list item for each curried argument.
                  val applicationTuple: List[TExp],
                  val applicataionType: TFunctionType,
                  // This is a list of the type environment application
                  // variables that can be removed.
                  val oldApplicationVariables: Set[TInternalIdentVar])
    extends InlineReason {
  def prettyPrint = ("Should be inlined.  Inlining cost is %s and the " +
    "threshold inlining cost is %s").stripMargin.format(inliningCost, thresh)
}

sealed trait InlineFailureReason extends InlineReason

case class VariableInline(val variableName: TNamedIdent)
    extends InlineFailureReason {
  def prettyPrint = """Cannot inline the variable %s because it appeared
    |not to be a function identifier.""".
    stripMargin.format(variableName.prettyPrint)
}

case class InlinePartialApplicationFailure(val name: TNamedIdent)
    extends InlineFailureReason {
  // In reality, there is no reason that we could not.  This is done
  // for simplicity of costing.
  def prettyPrint =
    "Cannot inline a partial function application of %s. ".format(
      name.prettyPrint)
}

case class InlineTooBigFailure(val name: TNamedIdent, val cost: Int,
                               val thresh: Int)
    extends InlineFailureReason {
  def prettyPrint =
    "Function %s was too big.  Estimated cost %s, threshold %s".format(
      name.prettyPrint, cost, thresh)
}

case class UninlinableFunction(val name: TIdent) 
    extends InlineFailureReason {
  def prettyPrint =
    "Funciton %s is not inlineable".format(name.prettyPrint)
}

case class UninlineableExpression(val exp: TExp)
    extends InlineFailureReason {
  def prettyPrint =
    "Function %s seems to not be a function".format(exp.prettyPrint)
}

case class InlineNotSupported(val reason: String)
    extends InlineFailureReason {
  def prettyPrint =
    reason
}
