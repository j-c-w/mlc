package toplev

import java.io.File
import java.nio.charset.StandardCharsets

import utils.FileUtils

// These passes are imported in the order they are used.
import frontend.GLLParser
import ast_change_names.ASTChangeNames
import typecheck.HindleyMilner
import lower_ast.LowerAST
import lambda_lift.LambdaLift

object Toplev extends App {
  val cli = new Arguments(args)
  val file = new File(cli.file())

  if (!file.exists) {
    println("Error, file " + file.toString + " doesn't exist")
  }

  Shared.filename = file.toString
  Shared.debug = cli.debug()

  // Frontend
  val code = FileUtils.readFileToString(file, StandardCharsets.UTF_8)
  val tree = GLLParser.execute(code, cli.dumpAst())
  val uniqueified = ASTChangeNames.execute(tree, cli.dumpChangeNames())
  val typechecked = HindleyMilner.execute(uniqueified, cli.dumpTypecheck())

  // Lowering from AST -- These all use the tree and so are OK
  // on memory.
  val intermediate = LowerAST.execute(typechecked, cli.dumpTir())
  val lambda_lifted = LambdaLift.execute(intermediate, cli.dumpLambdaLift())
}
