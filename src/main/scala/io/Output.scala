package io

import byteR.JVMProgram
import toplev._
import utils.FileUtils

object Output extends Pass[JVMProgram, CompileDone]("output") {
  def run(program: JVMProgram) = {
    val outputFileName = Shared.filename.replaceAll("\\.[^.]*$", "") + ".j"
    FileUtils.writeStringToFile(outputFileName, program.prettyPrint)
    dumpString(program.prettyPrint)
    new CompileDone()
  }
}
