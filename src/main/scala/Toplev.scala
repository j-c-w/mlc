package toplev

import java.io.File
import java.nio.charset.StandardCharsets

import utils.FileUtils
import target.JVMConfig

// These passes are imported in the order they are used.
import lexer.Lexer
import frontend.GLLParser
import ast_change_names.ASTChangeNames
import typecheck.HindleyMilner
import lower_ast.LowerAST
import lower_ast.LowerASTVerify
import lambda_lift.LambdaLift
import lambda_lift.LambdaLiftVerify
import lower_program.LowerProgram
import lower_program.LowerProgramVerify
import lower_variables.LowerVariablesPass
import lower_tir.LowerTIR
import peephole.Peephole

object Toplev extends App {
  val startTime = System.currentTimeMillis()
  val cli = new Arguments(args)
  val file = new File(cli.file)

  if (!file.exists) {
    println("Error, file " + file.toString + " doesn't exist")
    System.exit(1)
  }

  Shared.filename = file.toString
  Shared.debug = cli.debug
  Shared.targetConfig = JVMConfig

  // Frontend
  val code = FileUtils.readFileToString(file, StandardCharsets.UTF_8)
  val lexemes = Lexer.execute(code, cli.dumpLex)
  val tree = GLLParser.execute(lexemes, cli.dumpAst)
  val uniqueified = ASTChangeNames.execute(tree, cli.dumpChangeNames)
  val typechecked = HindleyMilner.execute(uniqueified, cli.dumpTypecheck)

  // Lowering from AST -- These all use the tree and so are OK
  // on memory.
  val intermediate = LowerAST.execute(typechecked, cli.dumpTir)
  val _0 = LowerASTVerify.optionalExecute(cli.runLowerAstVerify, intermediate,
                                          false)
  val lambda_lifted = LambdaLift.execute(intermediate, cli.dumpLambdaLift)
  val _1 = LambdaLiftVerify.optionalExecute(cli.runLambdaLiftVerify,
                                            lambda_lifted, false)

  // Optimizations on the TIR

  // Lower the TIR down into TIR+Assigns.
  val lowered_program = LowerProgram.execute(lambda_lifted,
                                             cli.dumpLowerProgram)
  val _2 = LowerProgramVerify.optionalExecute(cli.runLowerProgramVerify,
                                              lowered_program, false)

  // Optimizations on TIR+Assigns

  // Lower TIR+Assigns into byteR
  val numberedProgram = LowerVariablesPass.execute(lowered_program,
                                                   cli.dumpNumberedProgram)

  val byteR = LowerTIR.execute(numberedProgram, cli.dumpLowerTir)

  // Optimizations on byteR
  val postPeephole = Peephole.optionalExecute(cli.runPeephole, byteR,
                                              cli.dumpPeephole)
  
  // Output byteR
  val outputFileName = Shared.filename.replaceAll("\\.[^.]*$", "") + ".j"
  FileUtils.writeStringToFile(outputFileName, postPeephole.prettyPrint)

  if (cli.compileStats) {
    val endTime = System.currentTimeMillis()

    println("Compile time: " + (endTime - startTime) + "ms")
  }
}
