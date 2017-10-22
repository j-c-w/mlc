package exceptions

import frontend._

class SpecializationError(t1: ASTType, t2: ASTType) extends Exception("""
  Error: Could not specialize

  %s

  to

  %s

  """.format(t1.prettyPrint, t2.prettyPrint))
