package cmlc;

public abstract class LinkedList {
	public int length;
	public abstract boolean isNil();
	public abstract LinkedList append(LinkedList other);
	public abstract LinkedList cons(Object other);
	public abstract Object at(int i);
	public abstract Object head();
	public abstract LinkedList tail();
}
