package cmlc.lib.Math;

import cmlc.Function;

public class atan extends Function {
	public Object apply(Object input) {
		return (float) Math.atan((Float) input);
	}
}
