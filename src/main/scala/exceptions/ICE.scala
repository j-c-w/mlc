package exceptions

class ICE(val msg: String) extends RuntimeException(
  """Internal compiler error: %s
  | Please submit a bug report, including source if possible
  | or this trace if not. """.format(msg))
