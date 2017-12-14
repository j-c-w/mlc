package lexer

import toplev.GenericPrintable

case class LexemeSeq(val seq: Array[Lexeme]) extends GenericPrintable {
  def prettyPrint = seq.map(_.prettyPrint).mkString(", ")
}
