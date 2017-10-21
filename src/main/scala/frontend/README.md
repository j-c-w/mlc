This is the frontend to the compiler. It deals with parsing of input
and the definition of the AST (Abstract Syntax Tree) classes.

Definitions are found in files with names `AST*.scala`,
and the parser is found in GLLParser.scala.

This tree is not for optimization after typechecking!
Nodes like ASTExpLetIn and ASTExpFun have state not visisble
in the pattern match. DO NOT ATTEMPT TO REBUILD THE TREE or
this information will be lost.

The solution to this is to wait until TIR to do optimization.
Some conversions may be done in the lowering pass.
