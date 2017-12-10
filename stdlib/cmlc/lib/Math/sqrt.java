package cmlc.lib.Math;

import cmlc.Function;

public class sqrt extends Function {
	public Object apply(Object input) {
		return Math.sqrt((Float) input);
	}
}
