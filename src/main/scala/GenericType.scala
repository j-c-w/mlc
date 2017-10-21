package toplev

/* Types that are to be used within the machinery of
 * the compiler should extend this.
 *
 * This provides the verification and analysis
 * methods required for all parts of the compiler.
 *
 */

trait GenericType[TypeClass] {
  def unify(typ: TypeClass): TypeClass = ???

  def specializesTo(otherType: TypeClass): Boolean = ???

}
