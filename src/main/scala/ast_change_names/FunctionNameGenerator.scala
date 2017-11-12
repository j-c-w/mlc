package ast_change_names

object FunctionNameGenerator {
  var count = 0

  def newAnonymousName(): String = {
    count += 1

    "#" + nameFrom(count, "")
  }

  def newFunctionName(oldName: String) = {
    count += 1

    "$" + nameFrom(count, oldName)
  }

  def newIdentName(oldName: String) = {
    count += 1

    "@" + nameFrom(count, oldName)
  }

  def nameFrom(n: Int, prefix: String): String = n match {
    case 0 => "#" + prefix
    case n => "abcdefghjiklmnopqrstuvwxyz"(n % 26) + nameFrom(n / 26, prefix)
  }
}
