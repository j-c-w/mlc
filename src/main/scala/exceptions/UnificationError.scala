package exceptions

import toplev.GenericType

class UnificationError(val t1: GenericType[_], val t2: GenericType[_])
    extends Exception("""
  Error: could not unify types:

    %s

  with 

    %s
  """.format(t1.prettyPrint, t2.prettyPrint))
