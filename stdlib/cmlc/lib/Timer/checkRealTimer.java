package cmlc.lib.Timer;

import cmlc.Function;

/* This is a real hack.
 *
 * It  is only a stopgap so that benchmarking can be done with
 * times until something better is sorted.
 *
 * For compatability with MosML, the timer function works like this.
 *
 * But, since there are no datatypes in CMLC, we pretend this is a real
 * instead.  This causes problems since a float can't accurately store
 * time since the epoch! 
 *
 * The resolution is to take the current time at the program start and then
 * use that on all calls as a base time.
 *
 * A horrible, horrible hack.
 */

public class checkRealTimer extends Function {
	protected static final long zero = System.currentTimeMillis();

	public Object apply(Object input) {
		return ((float) (System.currentTimeMillis() - zero)) / 1000.0f -
			(Float) input;
	}
}
