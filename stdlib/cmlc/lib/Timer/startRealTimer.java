package cmlc.lib.Timer;

import cmlc.Function;

public class startRealTimer extends Function {
	public Object apply(Object input) {
		return ((float) (System.currentTimeMillis() -
						 checkRealTimer.zero)) / 1000.0f;
	}
}
