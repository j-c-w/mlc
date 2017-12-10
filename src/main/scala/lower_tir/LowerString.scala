package lower_tir

object LowerString {
  def apply(s: String) =
    // TODO -- ACTUALLY ESCAPE THE STRING.
    "'" + s + "'"
}
