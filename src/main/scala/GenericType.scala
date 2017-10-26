package toplev

/* Types that are to be used within the machinery of
 * the compiler should extend this.
 *
 * This provides the verification and analysis
 * methods required for all parts of the compiler.
 *
 */

trait GenericType[TypeClass <: GenericPrintable
                               with GenericType[TypeClass]]
    extends GenericPrintable {
  def unify(typ: TypeClass): GenericUnifier[TypeClass]

  def contains(otherType: TypeClass): Boolean

  def substituteFor(subFor: TypeClass, subIn: TypeClass): TypeClass

  def specializesTo(otherType: TypeClass): Boolean

  def getTypeVars(): TypeClassSet[TypeClass]

  def typeClone(): TypeClass
  
  def typeClone(types: TypeClassSet[TypeClass]): TypeClass
}
