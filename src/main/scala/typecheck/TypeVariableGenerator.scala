package typecheck

import frontend._
import tir._

/* This class generates unique type variables.
 */

object TypeVariableGenerator {
  private var tyVarId = 0: Int

  def getVar(): ASTUnconstrainedTypeVar = {
    tyVarId = tyVarId + 1

    return new ASTUnconstrainedTypeVar("$" + stringFor(tyVarId))
  }

  def getTVar(): TUnconstrainedTypeVar = {
    tyVarId = tyVarId + 1

    return new TUnconstrainedTypeVar("$" + stringFor(tyVarId))
  }

  def getEqualityVar(): ASTEqualityTypeVar = {
    tyVarId = tyVarId + 1
    
    ASTEqualityTypeVar("$" + stringFor(tyVarId))
  }

  def getVars(number: Int): List[ASTUnconstrainedTypeVar] = number match {
    case 0 => List[ASTUnconstrainedTypeVar]()
    case n => getVar() :: getVars(n - 1)
  }

  def getEqualityVars(number: Int): List[ASTEqualityTypeVar] =
  number match {
    case 0 => List[ASTEqualityTypeVar]()
    case n => getEqualityVar() :: getEqualityVars(n - 1)
  }

  def getNumberTypeVar(): ASTNumberType = {
    tyVarId = tyVarId + 1

    new ASTNumberType("$" + stringFor(tyVarId))
  }

  def getNumberTypeVars(number: Int): List[ASTNumberType] = number match {
    case 0 => List[ASTNumberType]()
    case n => getNumberTypeVar() :: getNumberTypeVars(n - 1)
  }

  def getComparableTypeVar(): ASTComparableType = {
    tyVarId = tyVarId + 1

    new ASTComparableType("$" + stringFor(tyVarId))
  }

  def getComparableTypeVars(number: Int): List[ASTComparableType] = number match {
    case 0 => List[ASTComparableType]()
    case n => getComparableTypeVar() :: getComparableTypeVars(n - 1)
  }

  def getIntStringTypeVar(): ASTIntStringType = {
    tyVarId = tyVarId + 1

    new ASTIntStringType("$" + stringFor(tyVarId))
  }

  def getIntStringTypeVars(number: Int): List[ASTIntStringType] = number match {
    case 0 => List[ASTIntStringType]()
    case n => getIntStringTypeVar() :: getIntStringTypeVars(n - 1)
  }

  def stringFor(id: Int): String = id match {
    case 0 => ""
    case n => "abcdefghijklmnopqrstuvwxyz"(n % 26) + stringFor(n / 26)
  }
}

