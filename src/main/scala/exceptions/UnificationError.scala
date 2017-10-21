package exceptions

import frontend.ASTType

class UnificationError(val t1: ASTType, val t2: ASTType) extends Exception("""
  Error: could not unify types:

    %s

  with 

    %s
  """.format(t1.prettyPrint, t2.prettyPrint))
