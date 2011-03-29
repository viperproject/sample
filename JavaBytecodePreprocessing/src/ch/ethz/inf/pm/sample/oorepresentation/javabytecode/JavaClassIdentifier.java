package ch.ethz.inf.pm.sample.oorepresentation.javabytecode;

import ch.ethz.inf.pm.sample.oorepresentation.ClassIdentifier;

/**
 *
 * A Java class identifier.
 * 
 * @author Roman Scheidegger
 */
public class JavaClassIdentifier implements ClassIdentifier {
    private String mName;
    
    public JavaClassIdentifier(String name) {
        this.mName = name;
    }

    public JavaType getThisType() {
        return null;
    }
}
