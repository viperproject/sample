package ch.ethz.inf.pm.sample.oorepresentation.javabytecode;

import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint;

/**
 * This class represents a ProgramPoint in Java Bytecode.
 *
 * @author Roman Scheidegger
 */
public class JavaProgramPoint extends ProgramPoint {
    String classname;
    String block;
    int row;

    /**
     *
     * @param nClassname The name of the class
     * @param nBlock
     * @param nRow
     */
    public JavaProgramPoint(String nClassname, String nBlock, int nRow) {
        this.classname = nClassname;
        this.block = nBlock;
        this.row = nRow;
    }

    /**
     * Is this JPP equal to the p2?
     * 
     * @param p2
     * @return
     */
    public boolean equals(JavaProgramPoint p2) {
        return (this.classname.equals(p2.classname) && this.block.equals(p2.block) && this.row == p2.row);
    }

    public int getColumn() {
        return 0;
    }

    public int getLine() {
        return row;
    }
}
