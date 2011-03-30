package ch.ethz.inf.pm.sample.userinterfaces;
import ch.ethz.inf.pm.sample.oorepresentation.*;
import ch.ethz.inf.pm.sample.abstractdomain.Analysis;
import ch.ethz.inf.pm.sample.abstractdomain.accesspermissions.*;
import ch.ethz.inf.pm.sample.oorepresentation.javabytecode.*;
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.*;

public class InstalledPlugins {
    public static ch.ethz.inf.pm.sample.oorepresentation.Compiler[] compilers;
    public static Analysis[] analyses;

    static  {
        compilers=new ch.ethz.inf.pm.sample.oorepresentation.Compiler[2];
        compilers[0]=new ScalaCompiler();
        compilers[1]=new JavaCompiler();

        analyses=new Analysis[1];
        analyses[0]=new AccessPermissionsAnalysis();
    }

}
