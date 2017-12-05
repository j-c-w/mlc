package typecheck

import frontend.{ASTIdentVar,ASTInternalIdent}
import tir.{TIdentVar,TTopLevelIdent,TInternalIdentVar}

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

  def newTVariable(): TIdentVar = {
    number += 1

    TIdentVar(getStringFor(number))
  }

  def newTInternalVariable(): TInternalIdentVar = {
    number += 1

    TInternalIdentVar(getStringFor(number))
  }

  def newTTopLevelVariable(): TTopLevelIdent = {
    number += 1

    TTopLevelIdent(getStringFor(number))
  }

  def getStringFor(num: Int): String = num match {
    case 0 => "%"
    case n =>  getStringFor(n / 26) + "abcdefghijklmnopqrstuvwxyz"(n % 26)
  }
}
