package cmlc;

public class CMLCThrowable extends RuntimeException {
	CMLCException exception;

	public CMLCThrowable(CMLCException exception) {
		super();
		this.exception = exception;
	}

	public CMLCException get() {
		return exception;
	}
}
