package cmlc;

public class Unit {

	@Override
	public boolean equals(Object other) {
		// Strictly speaking, Unit must be equal to everything
		// it is compared to (since it can only be compared to other
		// units).  However, it is hard to say when the JVM will use
		// this method internally...
		return other instanceof Unit;
	}
}
