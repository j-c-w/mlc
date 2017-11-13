package toplev

import scala.collection.mutable.{HashMap,Map}
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

  def getTypeVars(): GenericTypeSet[TypeClass]

  def atomicClone: TypeClass

  /* Given a map of substitutions to make, make those substitutions.
   */
  def substituteFor(substitutionMap: Map[TypeClass, TypeClass]): TypeClass


  /* This clones a type, creating a new type at each
   * but mainitaing the relationships between already unified
   * variables.
   */
  def typeClone: TypeClass = typeClone(getTypeVars())

  /* This does the same thing as the other typeClone,
   * but the only types changed are those that belong to the
   * set passed as an argument.
   */
  def typeClone(set: GenericTypeSet[TypeClass]): TypeClass = {
    val substitutionMap = new HashMap[TypeClass, TypeClass]()

    for (typ <- set) {
      val targetType = typ.atomicClone
      substitutionMap(typ) = targetType
    }

    substituteFor(substitutionMap)
  }
}
