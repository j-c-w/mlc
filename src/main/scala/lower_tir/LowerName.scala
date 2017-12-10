package lower_tir

/* The JVM does not support names with '@' in them.
 *
 * We lower the internal names by replacing '@' with '_'
 * and '#' with '_'
 */

object LowerName {
  def apply(name: String) =
    // - is not allowed as the first character of the string but it may be used
    // to replace "'", which can only occur within the identifier.
    name.replaceAll("[@#]", "_").replaceAll("[']", "-")
}
