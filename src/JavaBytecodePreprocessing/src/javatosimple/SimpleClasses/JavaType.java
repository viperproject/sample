package javatosimple.SimpleClasses;

import ch.ethz.inf.pm.sample.oorepresentation.Type;
import scala.collection.immutable.Set;


/**
 *
 * This class represents types.
 *
 * @author Roman Scheidegger
 */
// BIGNOTE: naive implementation
public class JavaType implements Type {
    private String mType;
    private boolean mStatic = false;

    public JavaType(String nType) {
        this.mType = nType;
    }

    public JavaType(String nType, boolean nStatic) {
        this.mType = nType;
        this.mStatic = nStatic;
    }
    /* methods from Type interface */
    public boolean isBottomExcluding(Set paramSet) {
        return true;
    }
    
    public scala.collection.immutable.List<String> getPossibleFields() {
    	return null;
    }
    
    public String getName() {
        return this.mType;
    }
    public boolean isStatic() {
        return mStatic;
    }
    public boolean isNumericalType() {
        return true;
    }
    public boolean isObject() {
        return true;
    }
    /* methods from Lattice interface */
    public Type bottom() {
        return null;
    }
    public Type factory() {
        return null;
    }
    public Type glb(Type left, Type right) {
        return null;
    }
    public boolean lessEqual(Type r) {
        return true;
    }
    public Type lub(Type left, Type right) {
        return null;
    }
    public Type top() {
        return null;
    }
    public Type widening(Type left, Type right) {
        return null;
    }
    public String toString() {
    	return mType;
    }
}
