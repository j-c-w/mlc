package cmlc;

public class Nil extends LinkedList {
	public Nil() {
		length = 0;
	}

	public boolean isNil() {
		return true;
	}

	public Object at(int i) {
		throw new RuntimeException("List index out of range");
	}

	public Object head() {
		throw new RuntimeException("Head of empty list");
	}

	public LinkedList tail() {
		throw new RuntimeException("Tail of empty list");
	}

	public LinkedList append(LinkedList other) {
		return other;
	}

	public LinkedList cons(Object other) {
		return new LinkedListNode(other, this);
	}

	@Override
	public boolean equals(Object other) {
		return other instanceof Nil;
	}
}
