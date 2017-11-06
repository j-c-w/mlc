package typecheck

import frontend.ASTIdentVar

object VariableGenertor {
  private var number: Int = 0

  def newVariable(): ASTIdentVar = {
    number += 1

    ASTIdentVar(getStringFor(number))
  }

  def getStringFor(num: Int): String = num match {
    case 0 => "%"
    case n =>  getStringFor(n / 26) + "abcdefghijklmnopqrstuvwxyz"(n % 26)
  }
}
