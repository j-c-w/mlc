package toplev

import java.io.File
import java.nio.charset.StandardCharsets

import utils.FileUtils

// These passes are imported in the order they are used.
import frontend.GLLParser
import typecheck.HindleyMilner
import lower_ast.LowerAST

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
  val typechecked = HindleyMilner.execute(tree, cli.dumpTypecheck())

  // Lowering from AST
  val intermediate = LowerAST.execute(tree, cli.dumpTir())
}
