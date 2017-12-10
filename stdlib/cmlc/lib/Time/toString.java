package cmlc.lib.Time;

import cmlc.Function;

public class toString extends Function {
	public Object apply(Object input) {
		return Float.toString((Float) input);
	}
}
