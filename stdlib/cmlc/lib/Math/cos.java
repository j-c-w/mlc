package cmlc.lib.Math;

import cmlc.Function;

public class cos extends Function {
	public Object apply(Object input) {
		return Math.cos((Float) input);
	}
}
