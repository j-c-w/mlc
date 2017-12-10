package cmlc;

public class Tuple {
	Object[] elements;

	public Tuple(int size) {
		elements = new Object[size];
	}

	public Object get(int index) {
		return elements[index];
	}

	public void set(int index, Object object) {
		elements[index] = object;
	}

	@Override
	public boolean equals(Object other) {
		if (!(other instanceof Tuple)) {
			return false;
		}

		Tuple otherTuple = (Tuple) other;
		if (otherTuple.elements.length != this.elements.length) {
			return false;
		}

		for (int i = 0; i < elements.length; i ++) {
			if (!elements[i].equals(otherTuple.elements[i])) {
				return false;
			}
		}

		return true;
	}
}
