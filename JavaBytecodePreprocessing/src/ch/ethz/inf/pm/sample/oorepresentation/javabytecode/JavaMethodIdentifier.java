package ch.ethz.inf.pm.sample.oorepresentation.javabytecode;

import ch.ethz.inf.pm.sample.oorepresentation.MethodIdentifier;

/**
 *
 * A Java method identifier.
 * 
 * @author Roman Scheidegger
 */
public class JavaMethodIdentifier implements MethodIdentifier {
    private String mName;

    public JavaMethodIdentifier(String name) {
        this.mName = name;
    }
    public String getName() {
    	return mName;
    }
}
