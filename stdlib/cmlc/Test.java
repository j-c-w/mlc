package cmlc;

class  Test {
	public static void main(String args[]) {
		Nil tail = new Nil();
		LinkedListNode n0 = new LinkedListNode(1, tail);
		LinkedListNode n1 = new LinkedListNode(2, n0);
		LinkedListNode n2 = new LinkedListNode(3, n1);

		assert(n2.length == 3);
		assert(n2.head() .equals(3));
		assert(n2.tail().head() .equals(2));
	}
}
