package ch.ethz.inf.pm.sample.oorepresentation.javabytecode;

import ch.ethz.inf.pm.sample.oorepresentation.PackageIdentifier;

/**
 * A Java package identifier.
 *
 * @author Roman Scheidegger
 */
public class JavaPackageIdentifier implements PackageIdentifier {
    private String mName;

    public JavaPackageIdentifier(String name) {
        this.mName = name;
    }
}
