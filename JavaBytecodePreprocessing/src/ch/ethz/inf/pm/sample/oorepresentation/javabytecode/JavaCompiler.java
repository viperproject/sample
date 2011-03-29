package ch.ethz.inf.pm.sample.oorepresentation.javabytecode;

import ch.ethz.inf.pm.sample.oorepresentation.ClassDefinition;

import java.io.File;

/**
 *
 * @author Scheidegger Roman
 */
public class JavaCompiler implements ch.ethz.inf.pm.sample.oorepresentation.Compiler {

    static int good=0;
    static int bad=0;
    static long time=0;

    scala.collection.immutable.List<ClassDefinition> list =scala.collection.immutable.List.empty();

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws Exception {
        new JavaCompiler().parseFile(new File("C:\\Users\\Pietro\\Desktop\\fdas"));
        System.out.print("done\nGood:"+good+"\nBad:"+bad+"\nTime:"+time+"\n");
    }

    public scala.collection.immutable.List<ClassDefinition> compileFile(String path) {
        this.parseFile(new File(path));
        return list;
    }

    public String getLabel() {
        return "Java bytecode";
    }

    private void parseFile(File f) {
        if(f.isDirectory()) {
            for(File f1 : f.listFiles())
                this.parseFile(f1);
        }
        else this.parseSingleFile(f.toString());
    }

    private void parseSingleFile(String name) {
        try {
        	System.out.println("File "+name);
        	long t = System.currentTimeMillis();
            list = list.$colon$colon(new ClassFileParser(name).getClassDefinition());
            time+=System.currentTimeMillis()-t;
            good++;
        }
        catch(Exception e) {bad++;}
        catch(Error e) {bad++;}
    }
}
