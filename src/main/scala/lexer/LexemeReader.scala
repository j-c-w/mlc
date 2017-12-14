package lexer

import scala.util.parsing.input._

class LexemeReader(tokens: Seq[Lexeme]) extends Reader[Lexeme] {
  override def first: Lexeme = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Lexeme] = new LexemeReader(tokens.tail)
}
