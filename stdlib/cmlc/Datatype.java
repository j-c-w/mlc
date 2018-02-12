package cmlc;

public abstract class Datatype extends Function {
	public Object value;

	public Datatype() { }

	public Object apply(Object value) {
		this.value = value;
		return this;
	}

	public Object unapply() {
		return value;
	}
}
