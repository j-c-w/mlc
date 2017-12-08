package cmlc;

public class LinkedListNode extends LinkedList {
	LinkedList tail;
	Object head;

	public LinkedListNode(Object head, LinkedList tail) {
		if (tail != null)
			this.length = tail.length + 1;

		this.tail = tail;
		this.head = head;
	}

	public boolean isNil() {
		return false;
	}

	public Object head() {
		return head;
	}

	public LinkedList tail() {
		return tail;
	}

	public Object at(int i) {
		if (i == 0) {
			return this.head;
		} else {
			return this.tail.at(i - 1);
		}
	}

	public LinkedListNode append(LinkedList other) {
		// Create a new node since this is immuable.
		// It may take a pointer to the same object since
		// that is also immutable.
		LinkedList list = this;
		LinkedListNode newList = new LinkedListNode(this.head, null);
		newList.length = other.length + list.length;

		LinkedListNode previousElementOfNewList = null;

		while (!list.isNil()) {
			previousElementOfNewList = newList;
			// Copy this node
			newList =
				new LinkedListNode(((LinkedListNode) list).head, null);
			// Set the length of this new node:
			newList.length = list.length + other.length;
			// Set the tail of the last node to this copied node.
			previousElementOfNewList.tail = newList;


			// Walk through this list
			list = ((LinkedListNode) list).tail;
		}

		// Set the last element to be the new list:
		newList.tail = other;

		return newList;
	}

	public LinkedList cons(Object newObject) {
		return new LinkedListNode(newObject, this);
	}

	@Override
	public boolean equals(Object other) {
		if (!(other instanceof LinkedListNode)) {
			return false;
		}

		LinkedListNode otherCast = (LinkedListNode) other;

		return otherCast.head.equals(this.head)
			&& otherCast.tail.equals(this.tail);
	}
}
