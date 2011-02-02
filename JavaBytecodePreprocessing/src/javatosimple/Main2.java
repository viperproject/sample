package javatosimple;

import ch.ethz.inf.pm.sample.oorepresentation.*;
import ch.ethz.inf.pm.sample.gui.*;
import javatosimple.SimpleClasses.JavaMethodIdentifier;
import java.util.*;
import java.io.*;


/**
 *
 * @author Scheidegger Roman
 */
public class Main2 {

    public static void main(String[] args) {
        ClassDefinition cd=Main2.parseSingleFile("C:\\Users\\Pietro\\Desktop\\ScalaImplementation\\JavaToSimple\\bin\\Example\\Temp.class");
        scala.collection.immutable.List<MethodDeclaration> methods=(scala.collection.immutable.List<MethodDeclaration>) cd.methods();
        for(int i=0; i < methods.size(); i++) {
        	MethodDeclaration method = methods.apply(i);
        	if(((JavaMethodIdentifier) method.name()).getName().equals("foo")) {
        		ControlFlowGraph cfg=method.body();
        		ShowGraph.showControlFlowGraph(cfg);
        		System.out.println(cfg.toString());
        	}
        }
    }
   

    private static ClassDefinition parseSingleFile(String name) {
        try {
        	System.out.println("File "+name);
            return new ClassFileParser(name).getClassDefinition();
        }
        catch(Exception e) {System.out.println("FAILED!");return null;}
    };
}
