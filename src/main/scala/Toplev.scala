package toplev

import java.io.File
import target.JVMConfig

// These passes are imported in the order they are used.
import io.Input
import lexer.Lexer
import frontend.GLLParser
import ast_change_names.ASTChangeNames
import typecheck.HindleyMilner
import lower_ast.LowerAST
import lower_ast.LowerASTVerify
import lift_declarations.LiftDecs
import lambda_lift.LambdaLift
import lambda_lift.LambdaLiftVerify
import tail_call_elim.TailCallElimination
import t_inline.TInline
import t_inline.TInlineVerify
import simplify.PreLowerSimplify
import lower_program.LowerProgram
import lower_program.LowerProgramVerify
import simplify.Simplify
import copy_prop.TCopyPropagation
import lower_variables.LowerVariablesPass
import lower_tir.LowerTIR
import byte_dce.ByteDCE
import peephole.Peephole
import io.Output

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
  Shared.compileStats = cli.compileStats

  // Frontend
  val code = Input.execute(file, cli.dumpInput)
  val lexemes = Lexer.execute(code, cli.dumpLex)
  val tree = GLLParser.execute(lexemes, cli.dumpAst)
  val uniqueified = ASTChangeNames.execute(tree, cli.dumpChangeNames)
  val typechecked = HindleyMilner.execute(uniqueified, cli.dumpTypecheck)

  // Lowering from AST -- These all use the tree and so are OK
  // on memory.
  val intermediate = LowerAST.execute(typechecked, cli.dumpTir)
  val _0 = LowerASTVerify.optionalExecute(cli.runLowerAstVerify, intermediate,
                                          false)
  val declaration_lifted = LiftDecs.execute(intermediate, cli.dumpDecLift)
  val lambda_lifted = LambdaLift.execute(declaration_lifted,
                                         cli.dumpLambdaLift)
  val _1 = LambdaLiftVerify.optionalExecute(cli.runLambdaLiftVerify,
                                            lambda_lifted, false)

  // Optimizations on the TIR
  val tail_call = TailCallElimination.optionalExecute(cli.runTce,
                                                      lambda_lifted,
                                                      cli.dumpTce)

  val inlined = TInline.optionalExecute(cli.runTInline, tail_call,
                                        cli.dumpTInline)
  val _2 = TInlineVerify.optionalExecute(cli.runTInlineVerify, inlined, false)

  val t_simplified = PreLowerSimplify.optionalExecute(cli.runPreLowerSimplify,
                                                      inlined,
                                                      cli.dumpSimplify)
  // Lower the TIR down into TIR+Assigns.
  val lowered_program = LowerProgram.execute(lambda_lifted,
                                             cli.dumpLowerProgram)
  val _3 = LowerProgramVerify.optionalExecute(cli.runLowerProgramVerify,
                                              lowered_program, false)

  // Optimizations on TIR+Assigns
  val copy_proped = TCopyPropagation.optionalExecute(cli.runTCopyProp,
                                                     lowered_program,
                                                     cli.dumpCopyProp)
  val simplified = Simplify.optionalExecute(cli.runPostLowerSimplify,
                                            copy_proped,
                                            cli.dumpSimplify)

  // Lower TIR+Assigns into byteR
  val numberedProgram = LowerVariablesPass.execute(simplified,
                                                   cli.dumpNumberedProgram)

  val byteR = LowerTIR.execute(numberedProgram, cli.dumpLowerTir)

  val postDCE = ByteDCE.optionalExecute(cli.runByteDce, byteR, cli.dumpByteDce)

  // Optimizations on byteR
  val postPeephole = Peephole.optionalExecute(cli.runPeephole, postDCE,
                                              cli.dumpPeephole)
  
  // Output byteR
  Output.execute(postPeephole, cli.dumpOutput)

  if (cli.compileStats) {
    val endTime = System.currentTimeMillis()

    println("Compile time: " + (endTime - startTime) + "ms")
  }
}
