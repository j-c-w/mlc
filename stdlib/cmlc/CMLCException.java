package cmlc;

public class CMLCException extends Datatype {
	public CMLCThrowable getThrowable() {
		return new CMLCThrowable(this);
	}
}
