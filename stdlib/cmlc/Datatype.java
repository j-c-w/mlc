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

	public boolean equals(Object other) {
		if (!(other instanceof Datatype)) {
			return false;
		} else {
			Datatype otherDatatype = (Datatype) other;

			if (otherDatatype.id() != id()) {
				return false;
			}

			Object thisUnapply = this.unapply();
			Object otherUnapply = otherDatatype.unapply();

			if (thisUnapply == null && otherUnapply == null) {
				return true;
			} else if (thisUnapply == null || otherUnapply == null) {
				return false;
			} else {
				return thisUnapply.equals(otherUnapply);
			}
		}
	}

	/* This is to return a different ID for each datatype instance
	 * implemented.  */
	public abstract int id();
}
