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
}
