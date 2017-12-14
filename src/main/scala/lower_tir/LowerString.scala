package lower_tir

import org.apache.commons.lang._

object LowerString {
  def apply(s: String) = {
    val escapedControlCharacters = StringEscapeUtils.escapeJava(s)
    val escapedDashes = escapedControlCharacters.replaceAll("'", "\\\\'")
    "'" + escapedDashes + "'"
  }
}
