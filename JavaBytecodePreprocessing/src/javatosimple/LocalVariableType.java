package javatosimple;

import javatosimple.SimpleClasses.JavaType;
/**
 *
 * A little helper class to keep track on actual variable declaration status.
 *
 * @author Roman Scheidegger
 */
public class LocalVariableType {
    JavaType type;
    int revision;

    /**
     * Initialize with a given type and set revision to 0.
     * @param newType
     */
    public LocalVariableType(String newType) {
        this.type = new JavaType(newType);
        this.revision = 0;
    }

    /**
     * Retrieve actual revision.
     * @return
     */
    public int getRevision() {
        return this.revision;
    }

    /**
     * The variable was redeclared so increase the revision.
     */
    private void redeclare() {
        this.revision++;
    }

    /**
     * Retrieve the current type as a string.
     *
     * @return
     */
    public String getType() {
        return this.type.toString();
    }

    /**
     * Retrieve the current type as a JavaType.
     *
     * @return
     */
    public JavaType getJavaType() {
        return this.type;
    }

    /**
     * Change the type.
     * 
     * @param newType
     */
    public void setType(String newType) {
        this.type = new JavaType(newType);
        this.redeclare();
    }
}
