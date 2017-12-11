package cmlc.lib.Math;

import cmlc.Function;

public class sin extends Function {
	public Object apply(Object input) {
		return (float) Math.sin((Float) input);
	}
}
