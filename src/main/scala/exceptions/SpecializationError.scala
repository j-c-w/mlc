package exceptions

import toplev.GenericType

class SpecializationError(t1: GenericType[_], t2: GenericType[_])
    extends Exception("""
  Error: Could not specialize

  %s

  to

  %s

  """.format(t1.prettyPrint, t2.prettyPrint))
