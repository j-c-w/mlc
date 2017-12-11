package cmlc.lib.Real;

import cmlc.Function;

public class fromInt extends Function {
	public Object apply(Object input) {
		return (Float) ((float) ((Integer) input));
	}
}
