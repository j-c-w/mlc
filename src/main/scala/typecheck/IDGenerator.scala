package typecheck

object IDGenerator {
  private var id = 0

  def newWhileLoopID = {
    id += 1

    id
  }
}
