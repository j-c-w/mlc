package peephole

sealed trait PeepholeRange

case class SequenceRequest(var start: Int, var end: Int) extends PeepholeRange
