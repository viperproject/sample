package javatosimple;

import ch.ethz.inf.pm.sample.oorepresentation.*;
import javatosimple.SimpleClasses.JavaType;

/**
 *
 * A helper class to enable storing of a Statement with type on the operand
 * stack.
 *
 * @author Roman Scheidegger
 */
public class TypedStatement {
    // statement + type
    private Statement sta;
    private JavaType type;

    public TypedStatement(Statement pSta, JavaType pType) {
        this.sta = pSta;
        this.type = pType;
    }

    public Statement getStatement() {
        return this.sta;
    }

    public JavaType getType() {
        return this.type;
    }

}
