package javatosimple;

import ch.ethz.inf.pm.sample.oorepresentation.ClassDefinition;
import java.io.*;


/**
 *
 * @author Scheidegger Roman
 */
public class Main {

    static int good=0;
    static int bad=0;
    static long time=0;
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        Main.parseFile(new File("Y:\\eth\\bachelor\\bachelorthesis\\rt\\sun\\nio\\ch\\IOVecWrapper.class"));
        System.out.print("done\nGood:"+good+"\nBad:"+bad+"\nTime:"+time+"\n");
    }
   

    private static void parseFile(File f) {
        if(f.isDirectory()) {
            for(File f1 : f.listFiles())
                Main.parseFile(f1);
        }
        else Main.parseSingleFile(f.toString());
    }
   
    private static void parseSingleFile(String name) {
        try {
        	System.out.println("File "+name);
        	long t = System.currentTimeMillis();
            new ClassFileParser(name);
            time+=System.currentTimeMillis()-t;
            good++;
        }
        catch(Exception e) {bad++;}
    }
}
