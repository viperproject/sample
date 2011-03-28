package javatosimple;

import ch.ethz.inf.pm.sample.oorepresentation.*;
//import scala.collection.immutable.List;

import java.util.*;


/**
 * A little helper class to extract Types of a given Java method descriptor or to
 * retrieve access modifiers.
 *
 * @author Roman Scheidegger
 */
public class TypeExtractor {
    // list of argument types
    private ArrayList<String> pTypes;
    // java descriptor and return type
    private String mDesc, rType;

    public TypeExtractor(String desc) {
        this.setDescriptor(desc);
    }

    /**
     * Parses the given descriptor and store the results in the argument list
     * and the return type.
     *
     * @param desc Java descriptor
     */
    public void setDescriptor(String desc) {
        // initialize parameter list and store given descriptor
        pTypes = new ArrayList<String>();
        this.mDesc = desc;

        // extract return type
        this.rType = this.mDesc.substring(this.mDesc.indexOf(")")+1);

        // extract each argument type
        String temp = this.mDesc.substring(this.mDesc.indexOf('(')+1, this.mDesc.indexOf(')'));
        for(int i = 0; i < temp.length(); i++ ) {
            if(temp.charAt(i) == 'L') {
                pTypes.add(temp.substring(i,temp.indexOf(';', i)+1));
                i = temp.indexOf(';',i);
            }
            else if(temp.charAt(i) == '[') {
                int j = i+1;
                while(temp.charAt(j) == '[')
                    j++;
                if(temp.charAt(j) == 'L') {
                    pTypes.add(temp.substring(i,temp.indexOf(';', j)+1));
                    i = temp.indexOf(';',j);
                }
                else {
                    pTypes.add(temp.substring(i,j+1));
                    i = j;
                }
            }
            else {
                pTypes.add(temp.substring(i, i+1));
            }
        }
    }

    /**
     * Retrieve the return type of the set descriptor.
     * @return
     */
    public String getReturnType() {
        return this.rType;
    }

    /**
     * Retrieve the i-th argument type of the set descriptor.
     * @param i
     * @return
     */
    public String getParamterType(int i) {
        return this.pTypes.get(i);
    }

    /**
     * Get the count of arguments of the set descriptor.
     * @return
     */
    public int getParameterCount() {
        return this.pTypes.size();
    }

    /**
     * Transforms a Java object type given like java.util.Stack to a form like
     * Ljava/util/Stack;
     * @param type
     * @return
     */
    public static String objectType(String type) {
        return "L" + type.replace(".", "/") + ";";
    }

    /**
     * This helper function actually translates a integer accessflag to a scala
     * list of access modifiers.
     *
     * @param accessflags
     * @return Scala list of access modifiers
     */
    public static scala.collection.immutable.List<Modifier> getModifiers(int accessflags) {
    	scala.collection.immutable.List<Modifier> mods = scala.collection.immutable.List.empty();
        try {
            if((accessflags & 1024) != 0)
                mods = mods.$colon$colon((Modifier)AbstractModifier$.MODULE$);
            if((accessflags & 1) != 0)
                mods = mods.$colon$colon((Modifier)PublicModifier$.MODULE$);
            if((accessflags & 2) != 0)
                mods = mods.$colon$colon((Modifier)PrivateModifier$.MODULE$);
            if((accessflags & 4) != 0)
                mods = mods.$colon$colon((Modifier)ProtectedModifier$.MODULE$);
            if((accessflags & 16) != 0)
                mods = mods.$colon$colon((Modifier)FinalModifier$.MODULE$);
            // BIGNOTE: trait as interface
            if((accessflags & 512) != 0)
                mods = mods.$colon$colon((Modifier)TraitModifier$.MODULE$);
            // BIGNOTE: there is no ObjectModifier for static
        }
        catch(Exception e) {
            System.out.print("Exception: " + e.toString() + "\n");
        }
        return mods;
        /* list of accessflag values
        ANNOTATION 	8192
        BRIDGE 	64
        ENUM 	16384
        FINAL 	16
        INTERFACE 	512
        NATIVE 	256
        PRIVATE 	2
        PROTECTED 	4
        PUBLIC 	1
        STATIC 	8
        STRICT 	2048
        SUPER 	32
        SYNCHRONIZED 	32
        SYNTHETIC 	4096
        TRANSIENT 	128
        VARARGS 	128
        VOLATILE 	64
        */
    }

}
