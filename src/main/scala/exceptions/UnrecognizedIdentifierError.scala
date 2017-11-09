package exceptions

class UnrecognizedIdentifierError(val msg: String)
    extends RuntimeException(msg)
