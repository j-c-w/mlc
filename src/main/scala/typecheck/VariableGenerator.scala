package typecheck

import frontend.{ASTIdentVar,ASTInternalIdent}
import tir._

object VariableGenerator {
  private var number: Int = 0

  def newVariable(): ASTIdentVar = {
    number += 1

    ASTIdentVar(getStringFor(number))
  }

  def newInternalVariable(): ASTInternalIdent = {
    number += 1

    ASTInternalIdent(getStringFor(number))
  }

  def newTVariable(varClass: TIdentClass): TIdentVar = {
    number += 1

    TIdentVar(getStringFor(number), varClass)
  }

  def newTInternalVariable(): TInternalIdentVar = {
    number += 1

    TInternalIdentVar(getStringFor(number))
  }

  def newTTopLevelVariable(varClass: TIdentClass): TTopLevelIdent = {
    number += 1

    TTopLevelIdent(getStringFor(number), varClass)
  }

  def newTMutableVariable(varClass: TIdentClass): TMutableIdent = {
    number += 1

    TMutableIdent(getStringFor(number), varClass)
  }

  def getStringFor(num: Int): String = num match {
    case 0 => "%"
    case n =>  getStringFor(n / 26) + "abcdefghijklmnopqrstuvwxyz"(n % 26)
  }
}
