package byte_walk

import byteR._
import scala.collection.mutable.{HashMap,HashSet}
import toplev.GenericPrintable


/* This is a structure for storing a CFG of the byteR representation.
 *
 * The jumpSet is as produced by the CFG building walk, a map of instruction
 * index at the start of a BB to the last instruction index in that BB
 * and the start indecies of any following BBs.
 */

class JVMCFG(val jumpSet: HashMap[Int, (Int, List[Int])],
             val instructions: Array[JVMInstruction])
    extends GenericPrintable {
  def getBB(from: Int, to: Int) =
    instructions.slice(from, to + 1)

  def prettyPrint = """
  |CFG:

  |%s
  """.stripMargin.format(jumpSet.map {
    case (from, (to, next)) =>
      "\nBB Number " + from + "\n" +
      "BB Range " + from + "-" + to + "\n\n" +
      getBB(from, to).map(_.prettyPrint).mkString("\n") +
      "\n\nNext BBs: " + next.mkString(",")
  }.mkString("\n\n---- BB Divide ----\n"))
}
