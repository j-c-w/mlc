package cmlc.lib.Real;

import cmlc.Function;

public class toInt extends Function {
	public Object apply(Object input) {
		return Math.floor((Float) input);
	}
}
