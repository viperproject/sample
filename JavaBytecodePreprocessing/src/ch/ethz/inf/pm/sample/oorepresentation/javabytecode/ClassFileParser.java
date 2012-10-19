package ch.ethz.inf.pm.sample.oorepresentation.javabytecode;

import ch.ethz.inf.pm.sample.abstractdomain.*;
import scala.*;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.DataInputStream;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Vector;
import java.util.Iterator;
import java.util.Stack;

import javassist.bytecode.ClassFile;
import javassist.bytecode.MethodInfo;
import javassist.bytecode.CodeIterator;
import javassist.bytecode.Mnemonic;

import ch.ethz.inf.pm.sample.oorepresentation.*;
import java.util.ArrayList;
import javassist.bytecode.AccessFlag;

import javassist.bytecode.ConstPool;
import javassist.bytecode.ExceptionTable;
import javassist.bytecode.FieldInfo;
import scala.None;
import scala.math.ScalaNumber;

/**
 * The ClassFileParser loads and parses a given Java classfile and generates a
 * ClassDefinition out of it which later can be analyzed by PietroAnalyzer.
 *
 * @author Roman Scheidegger
 */
public class ClassFileParser {
    private String mClassfileName;
    private ClassFile mClassfile;
    private ClassDefinition mClassdefinition;

    // virtual operand stack
    private Stack opsta;

    // instruction id stack stores the ids of unflushed bytecodes
    private Stack ins;

    // stack for managing JSR calls
    private Stack jsrcalls;

    // map for matching statement id to list of corresponding bytecode addresses
    private Map<Integer, List<String>> staidtoins;
    // map for matching bytecode address to statemend id
    private Map<String, Integer> instostaid;

    // maps storing the edges to be built in the end
    private Map<String, String> unconditionaljumps;
    private Map<String, String> truejumps;
    private Map<String, String> falsejumps;

    private ControlFlowGraph cfg;
    private List<Statement> parsedStatements;

    // list for tracking local variable types
    private List<LocalVariableType> localtypes;

    // adding method parameter types
    private TypeExtractor te;

    // main parsing loop, iterate through each bytecode instruction
    private CodeIterator ci;
    private ConstPool cp;

    private JavaProgramPoint jpp;

    // index of current bytecode instruction
    private int index;
    // current bytecode instruction
    private int op;
    // variable counter for introducing temporary variables
    private int tempfunctionvariable;
    // statement id
    private int staid;

    /**
     * The construltor just takes a Java classfile path and starts parsing the
     * classfile at this location.
     *
     * @param classfile
     * @throws Exception
     */
    public ClassFileParser(String classfile) throws Exception {
        this.mClassfileName = classfile;
        this.mClassfile = null;
        this.parseWholeClass();
    }

    /**
     * Retrieve the generated ClassDefinition
     * @return
     */
    public ClassDefinition getClassDefinition() {
        return this.mClassdefinition;
    }

    /**
     * Start parsing the whole classfile
     * @throws Exception
     */
    private void parseWholeClass() throws Exception {
        //System.out.print("***ClassFileParser*** trying to parse classfile : \n" + this.mClassfileName
        //        + "\n----------------------------------------------------------\n\n");

        // create a new javasisst classfile and constpool
        BufferedInputStream fin = new BufferedInputStream(new FileInputStream(mClassfileName));
        this.mClassfile = new ClassFile(new DataInputStream(fin));
        cp = mClassfile.getConstPool();

        // get AccessModifiers of given class
        scala.collection.immutable.List<Modifier> classmodifiers = TypeExtractor.getModifiers(this.mClassfile.getAccessFlags());

        // get superclass
        scala.collection.immutable.List<ClassIdentifier> extendedclasses = scala.collection.immutable.List.empty();
        extendedclasses = extendedclasses.$colon$colon((ClassIdentifier)new JavaClassIdentifier(this.mClassfile.getSuperclass()));

        // get package name
        String packageName = this.mClassfile.getName().substring(0, this.mClassfile.getName().lastIndexOf("."));

        // get FieldDeclarations of given class
        scala.collection.immutable.List<FieldDeclaration> fielddeclarations = scala.collection.immutable.List.empty();
        List<FieldInfo> fields = this.mClassfile.getFields();
        int fieldnum = 0;
        for (Iterator i = fields.iterator(); i.hasNext(); ) {
            FieldInfo fi = (FieldInfo)i.next();
            fieldnum++;
            NumericalConstant rv = null;
            int j = fi.getConstantValue();
            if((j) > 0) {
                Object rightvalue = cp.getLdcValue(j);

                if(rightvalue.getClass().getCanonicalName().equals("java.lang.String")) {
                    rv = new NumericalConstant(null, (String)rightvalue, new JavaType("Ljava/lang/String;"));
                }
                else if(rightvalue.getClass().getCanonicalName().equals("java.lang.Integer;")) {
                    rv = new NumericalConstant(null, Integer.toString((Integer)rightvalue), new JavaType("I"));
                }
                else if(rightvalue.getClass().getCanonicalName().equals("java.lang.Float;")) {
                    rv = new NumericalConstant(null, Float.toString((Float)rightvalue), new JavaType("F"));
                }
                else if(rightvalue.getClass().getCanonicalName().equals("java.lang.Long;")) {
                    rv = new NumericalConstant(null, Long.toString((Long)rightvalue), new JavaType("J"));
                }
                else if(rightvalue.getClass().getCanonicalName().equals("java.lang.Double;")) {
                    rv = new NumericalConstant(null, Double.toString((Double)rightvalue), new JavaType("D"));
                }
            }

            jpp = new JavaProgramPoint(this.mClassfileName, "::field_info", fieldnum-1);
            fielddeclarations = fielddeclarations.$colon$colon(
                    new FieldDeclaration(
                        jpp,
                        TypeExtractor.getModifiers(fi.getAccessFlags()),
                        //TODO: the variable identifier now requires a type.
                        new Variable(jpp, new VariableIdentifier(fi.getName(), new JavaType("TODO"), jpp)),
                        new JavaType(fi.getDescriptor()),
                        rv
                    )
            );
        }

        // get MethodDeclarations of given class
        List<MethodInfo> methods = this.mClassfile.getMethods();

        scala.collection.immutable.List<MethodDeclaration> methoddeclarations = scala.collection.immutable.List.empty();
        int methodnum = 0;
        for (Iterator i = methods.iterator(); i.hasNext(); ) {
            MethodInfo mi = (MethodInfo)i.next();
            methodnum++;
            // get VariableDeclarations of arguments of a given method
            scala.collection.immutable.List<VariableDeclaration> arguments = scala.collection.immutable.List.empty();
            te = new TypeExtractor(mi.getDescriptor());
            for(int j = 0; j < te.getParameterCount(); j++) {
                jpp =  new JavaProgramPoint(this.mClassfileName, "::method_info", methodnum-1);
                arguments = arguments.$colon$colon(
                        new VariableDeclaration(
                            jpp,
                            //TODO: the variable identifier now requires a type.
                            new Variable(
                                jpp,
                                new VariableIdentifier(mi.getName()+"arg#"+Integer.toString(j), new JavaType("TODO"), jpp)
                            ),
                            new JavaType(te.getParamterType(j)),
                            null
                        )
                );
            }

            // only parse the method if it isn't abstract or native
            ControlFlowGraph methodbody = null;
            if(((mi.getAccessFlags() & AccessFlag.ABSTRACT) == 0) && ((mi.getAccessFlags() & AccessFlag.NATIVE) == 0))
                methodbody = this.parseMethod(mi);

            methoddeclarations = methoddeclarations.$colon$colon(
                    new MethodDeclaration(
                        new JavaProgramPoint(this.mClassfileName, "::method_info", methodnum-1),
                        null,
                        TypeExtractor.getModifiers(mi.getAccessFlags()),
                        new JavaMethodIdentifier(mi.getName()),
                        null,
                        null,
                        null,
                        methodbody,
                        null,
                        null
                    )
            );
        }

        // generate ClassDefinition
        this.mClassdefinition = new ClassDefinition(
                new JavaProgramPoint(this.mClassfileName,"::classfile",0),
                null,
                classmodifiers,
                new JavaClassIdentifier(this.mClassfileName),
                null,
                extendedclasses,
                fielddeclarations,
                methoddeclarations,
                new JavaPackageIdentifier(packageName),
                null
        );

        //System.out.print("\n\n----------------------------------------------------------\n"
                //+ "***ClassFileParser*** done with parsing !\n\n");

    }

    /**
     * Generates the ControlFlowGraph for a given method.
     *
     * @param minfo The method is given as a javassist.MethodInfo
     * @return CFG
     * @throws Exception
     */
    public ControlFlowGraph parseMethod(MethodInfo minfo) throws Exception {
        String methodname = minfo.getName();
        //System.out.print("\n**PARSING METHOD: " + methodname + "\n\n");

        // setup our helping data structures
        opsta = new Stack();
        ins = new Stack();
        parsedStatements = new ArrayList();
        instostaid = new HashMap();
        staidtoins = new HashMap();
        unconditionaljumps = new HashMap();
        truejumps = new HashMap();
        falsejumps = new HashMap();
        jsrcalls = new Stack();

        // initialise our ControlFlowGraph
        cfg = new ControlFlowGraph(new JavaProgramPoint(this.mClassfileName, methodname, 0));

        // initialize list for tracking local variable types
        int ml = minfo.getCodeAttribute().getMaxLocals();
        localtypes = new Vector(ml);
        for(int i = 0; i<ml; i++)
            localtypes.add(null);

        // adding method parameter types
        te = new TypeExtractor(minfo.getDescriptor());
        int staoff = 0;

        // for non-static methods the first variable is 'this'
        if( (minfo.getAccessFlags() & AccessFlag.STATIC) == 0 ) {
            localtypes.set(0, new LocalVariableType(this.mClassfile.getName()));
            staoff = 1;
        }

        int in = 0;
        for(int i = 0; i < te.getParameterCount(); i++) {
            localtypes.set(in+staoff, new LocalVariableType(te.getParamterType(i)));
            // double and long use 'two variable' spots
            if(te.getParamterType(i).equals("J") || te.getParamterType(i).equals("D"))
                in++;
            in++;
        }


        // initialize helper variables
        ci = minfo.getCodeAttribute().iterator();
        JavaType statype = null;
        index = 0;
        int offset = 0;
        staid = 0;
        tempfunctionvariable = 0;
        // are we parsing a wide instruction
        boolean wide = false;

        // extract start of exception handlers
        ExceptionTable et = minfo.getCodeAttribute().getExceptionTable();
        List<Integer> exceptionIndexes = new ArrayList();
        for(int k = 0; k < et.size(); k++)
            exceptionIndexes.add(et.handlerPc(k));

        // main parsing loop, iterate through each bytecode instruction
        while (ci.hasNext()) {
            index = ci.next();

            // skip exception handler code if not target of a jump
            boolean breakparsing = false;
            if(exceptionIndexes.contains(index)) {
                while(!(unconditionaljumps.containsValue(__getactualindex(index)) || truejumps.containsValue(__getactualindex(index)) || falsejumps.containsValue(__getactualindex(index))))
                    if(ci.hasNext())
                        index = ci.next();
                    else {
                        breakparsing = true;
                        break;
                    }
            }
            if(breakparsing)
                break;

            // retriev opcode
            op = ci.byteAt(index);

            // push instruction ID
            ins.push(__getactualindex(index));

            // determine actual jpp
            jpp = new JavaProgramPoint(this.mClassfileName, methodname, index);

            // helping varariables to create Statements
            scala.collection.immutable.List<Statement> obj;
            scala.collection.immutable.List<Statement> param;
            TypedStatement op4, op3, op2, op1;
            int varindex;

            //System.out.println(index + ":\t" + Mnemonic.OPCODE[op]);

            switch(op) {
                // nop
                case 0:
                    break;
                // aconst_null
                case 1:
                    // BIGNOTE: what is the correct type of null?
                    statype = new JavaType("Ljava/lang/Object;");
                    opsta.push(new TypedStatement(new NumericalConstant(jpp, "null", statype),statype));
                    break;
                // iconst_m1
                case 2:
                // iconst_0
                case 3:
                // iconst_1
                case 4:
                // iconst_2
                case 5:
                // iconst_3
                case 6:
                // iconst_4
                case 7:
                // iconst_5
                case 8:
                    varindex = op-3;
                    __pushconstant(Integer.toString(varindex), "I");
                    break;
                // lconst_0
                case 9:
                // lconst_1
                case 10:
                    varindex = op-9;
                    __pushconstant(Integer.toString(varindex), "J");
                    break;
                // fconst_0
                case 11:
                // fconst_1
                case 12:
                // fconst_2
                case 13:
                    varindex = op-11;
                    __pushconstant(Float.toString((float)varindex), "F");
                    break;
                // dconst_0
                case 14:
                // dconst_1
                case 15:
                    varindex = op-14;
                    __pushconstant(Double.toString((double)varindex), "D");
                    break;
                // bipush
                case 16:
                    __pushconstant(Integer.toString(ci.byteAt(index+1)),"I");
                    break;
                // sipush
                case 17:
                    __pushconstant(Integer.toString(ci.s16bitAt(index+1)),"I");
                    break;
                // ldc
                case 18:
                // ldc_w
                case 19:
                // ldc2_w
                case 20:
                    if(op == 18)
                        offset = ci.byteAt(index+1);
                    else
                        offset = (ci.byteAt(index+1) << 8) | ci.byteAt(index+2);
                    Object ldcobject = cp.getLdcValue(offset);
                    // string references are stored as CONSTANT_ClassInfo
                    if(ldcobject == null)
                        ldcobject = cp.getClassInfo(offset);
                    if(ldcobject.getClass().getCanonicalName().equals("java.lang.String")) {
                        __pushconstant((String)ldcobject, "Ljava/lang/String;");
                    }
                    else if(ldcobject.getClass().getCanonicalName().equals("java.lang.Integer")) {
                        __pushconstant(Integer.toString((Integer)ldcobject),"I");
                    }
                    else if(ldcobject.getClass().getCanonicalName().equals("java.lang.Float")) {
                        __pushconstant(Float.toString((Float)ldcobject),"F");
                    }
                    else if(ldcobject.getClass().getCanonicalName().equals("java.lang.Long")) {
                        __pushconstant(Long.toString((Long)ldcobject),"J");
                    }
                    else if(ldcobject.getClass().getCanonicalName().equals("java.lang.Double")) {
                        __pushconstant(Double.toString((Double)ldcobject),"D");
                    }
                    break;
                // iload
                case 21:
                // lload
                case 22:
                // fload
                case 23:
                // dload
                case 24:
                // aload
                case 25:
                    if(wide) {
                        __pushvariable((ci.byteAt(index+1) << 8) | ci.byteAt(index+2));
                        wide = false;
                    }
                    else
                        __pushvariable(ci.byteAt(index+1));
                    break;
                // iload_0
                case 26:
                // iload_1
                case 27:
                // iload_2
                case 28:
                // iload_3
                case 29:
                    __pushvariable(op-26);
                    break;
                // lload_0
                case 30:
                // lload_1
                case 31:
                // lload_2
                case 32:
                // lload_3
                case 33:
                    __pushvariable(op-30);
                    break;
                // fload_0
                case 34:
                // fload_1
                case 35:
                // fload_2
                case 36:
                // fload_3
                case 37:
                    __pushvariable(op-34);
                    break;
                // dload_0
                case 38:
                // dload_1
                case 39:
                // dload_2
                case 40:
                // dload_3
                case 41:
                    __pushvariable(op-38);
                    break;
                // aload_0
                case 42:
                // aload_1
                case 43:
                // aload_2
                case 44:
                // aload_3
                case 45:
                    __pushvariable(op-42);
                    break;
                // iaload
                case 46:
                    __pusharrayaccessread("I");
                    break;
                // laload
                case 47:
                    __pusharrayaccessread("L");
                    break;
                // faload
                case 48:
                    __pusharrayaccessread("F");
                    break;
                // daload
                case 49:
                    __pusharrayaccessread("D");
                    break;
                // aaload
                case 50:
                    __pusharrayaccessread("Ljava/lang/Object;");
                    break;
                // baload
                case 51:
                    // PAY ATTENTION: different sign extension for byte or boolean
                    __pusharrayaccessread("I");
                    break;
                // caload
                case 52:
                    __pusharrayaccessread("I");
                    break;
                // saload
                case 53:
                    __pusharrayaccessread("I");
                    break;
                // istore
                case 54:
                // lstore
                case 55:
                // fstore
                case 56:
                // dstore
                case 57:
                // astore
                case 58:
                    if(wide) {
                        __variableassignment((ci.byteAt(index+1) << 8) | ci.byteAt(index+2));
                        wide = false;
                    }
                    else
                        __variableassignment(ci.byteAt(index+1));
                    break;
                // istore_0
                case 59:
                // istore_1
                case 60:
                // istore_2
                case 61:
                // istore_3
                case 62:
                    __variableassignment(op-59);
                    break;
                // lstore_0
                case 63:
                // lstore_1
                case 64:
                // lstore_2
                case 65:
                // lstore_3
                case 66:
                    __variableassignment(op-63);
                    break;
                // fstore_0
                case 67:
                // fstore_1
                case 68:
                // fstore_2
                case 69:
                // fstore_3
                case 70:
                    __variableassignment(op-67);
                    break;
                // dstore_0
                case 71:
                // dstore_1
                case 72:
                // dstore_2
                case 73:
                // dstore_3
                case 74:
                    __variableassignment(op-71);
                    break;
                // astore_0
                case 75:
                // astore_1
                case 76:
                // astore_2
                case 77:
                // astore_3
                case 78:
                    __variableassignment(op-75);
                    break;
                // iastore
                case 79:
                    __pusharrayaccesswrite("I");
                    break;
                // lastore
                case 80:
                    __pusharrayaccesswrite("J");
                    break;
                // fastore
                case 81:
                    __pusharrayaccesswrite("F");
                    break;
                // dastore
                case 82:
                    __pusharrayaccesswrite("D");
                    break;
                // aastore
                case 83:
                    __pusharrayaccesswrite("Ljava/lang/Object;");
                    break;
                // bastore
                case 84:
                    __pusharrayaccesswrite("((TypedStatement)opsta.peek()).getType().getName()");
                    break;
                // castore
                case 85:
                    __pusharrayaccesswrite("C");
                    break;
                // sastore
                case 86:
                    __pusharrayaccesswrite("S");
                    break;
                // pop
                case 87:
                    __flushids(((TypedStatement)opsta.pop()).getStatement(), false);
                    break;
                // pop2
                case 88:
                    JavaType poptype = ((TypedStatement)opsta.pop()).getType();
                    if(!(poptype.getName().equals("J") | poptype.getName().equals("D")))
                    __flushids(((TypedStatement)opsta.pop()).getStatement(),false);
                    break;
                // dup
                case 89:
                    op1 = ((TypedStatement)opsta.pop());

                    opsta.push(op1);
                    opsta.push(op1);
                    break;
                // dup_x1
                case 90:
                    op1 = ((TypedStatement)opsta.pop());
                    op2 = ((TypedStatement)opsta.pop());

                    opsta.push(op1);
                    opsta.push(op2);
                    opsta.push(op1);
                    break;
                // dup_x2
                case 91:
                    op1 = ((TypedStatement)opsta.pop());
                    op2 = ((TypedStatement)opsta.pop());
                    if(op2.getType().getName().equals("J") || op2.getType().getName().equals("D")) {
                        opsta.push(op1);
                        opsta.push(op2);
                        opsta.push(op1);
                    }
                    else {
                        op3 = ((TypedStatement)opsta.pop());
                        opsta.push(op1);
                        opsta.push(op3);
                        opsta.push(op2);
                        opsta.push(op1);
                    }
                    break;
                // dup2
                case 92:
                    op1 = ((TypedStatement)opsta.pop());
                    if(op1.getType().getName().equals("J") || op1.getType().getName().equals("D")) {
                        opsta.push(op1);
                        opsta.push(op1);
                    }
                    else {
                        op2 = ((TypedStatement)opsta.pop());
                        opsta.push(op2);
                        opsta.push(op1);
                        opsta.push(op2);
                        opsta.push(op1);
                    }
                    break;
                // dup2_x1
                case 93:
                    op1 = ((TypedStatement)opsta.pop());
                    op2 = ((TypedStatement)opsta.pop());
                    if(op1.getType().getName().equals("J") || op1.getType().getName().equals("D")) {
                        opsta.push(op1);
                        opsta.push(op2);
                        opsta.push(op1);
                    }
                    else {
                        op3 = ((TypedStatement)opsta.pop());
                        opsta.push(op2);
                        opsta.push(op1);
                        opsta.push(op3);
                        opsta.push(op2);
                        opsta.push(op1);
                    }
                    break;
                // dup2_x2
                case 94:
                    op1 = ((TypedStatement)opsta.pop());
                    op2 = ((TypedStatement)opsta.pop());
                    if(op1.getType().getName().equals("J") || op1.getType().getName().equals("D")) {
                        if(op2.getType().getName().equals("J") || op2.getType().getName().equals("D")) {
                            opsta.push(op1);
                            opsta.push(op2);
                            opsta.push(op1);
                        }
                        else {
                            op3 = ((TypedStatement)opsta.pop());
                            opsta.push(op1);
                            opsta.push(op3);
                            opsta.push(op2);
                            opsta.push(op1);
                        }
                    }
                    else {
                        op3 = ((TypedStatement)opsta.pop());
                        if(op3.getType().getName().equals("J") || op3.getType().getName().equals("D")) {
                            opsta.push(op2);
                            opsta.push(op1);
                            opsta.push(op3);
                            opsta.push(op2);
                            opsta.push(op1);
                        }
                        else {
                            op4 = ((TypedStatement)opsta.pop());
                            opsta.push(op2);
                            opsta.push(op1);
                            opsta.push(op4);
                            opsta.push(op3);
                            opsta.push(op2);
                            opsta.push(op1);
                        }
                    }
                    break;
                // swap
                case 95:
                    op1 = ((TypedStatement)opsta.pop());
                    op2 = ((TypedStatement)opsta.pop());
                    opsta.push(op1);
                    opsta.push(op2);
                    break;
                // iadd
                case 96:
                    __pushbioperator("+","I");
                    break;
                // ladd
                case 97:
                    __pushbioperator("+","J");
                    break;
                // fadd
                case 98:
                    __pushbioperator("+","F");
                    break;
                // dadd
                case 99:
                    __pushbioperator("+","D");
                    break;
                // isub
                case 100:
                    __pushbioperator("-","I");
                    break;
                // lsub
                case 101:
                    __pushbioperator("-","J");
                    break;
                // fsub
                case 102:
                    __pushbioperator("-","F");
                    break;
                // dsub
                case 103:
                    __pushbioperator("-","D");
                    break;
                // imul
                case 104:
                    __pushbioperator("*","I");
                    break;
                // lmul
                case 105:
                    __pushbioperator("*","J");
                    break;
                // fmul
                case 106:
                    __pushbioperator("*","F");
                    break;
                // dmul
                case 107:
                    __pushbioperator("*","D");
                    break;
                // idiv
                case 108:
                    __pushbioperator("/","I");
                    break;
                // ldiv
                case 109:
                    __pushbioperator("/","J");
                    break;
                // fdiv
                case 110:
                    __pushbioperator("/","F");
                    break;
                // ddiv
                case 111:
                    __pushbioperator("/","D");
                    break;
                // irem
                case 112:
                    __pushbioperator("%","I");
                    break;
                // lrem
                case 113:
                    __pushbioperator("%","J");
                    break;
                // frem
                case 114:
                    __pushbioperator("%","F");
                    break;
                // drem
                case 115:
                    __pushbioperator("%","D");
                    break;
                // ineg
                case 116:
                    __pushoperator("negate","I");
                    break;
                // lneg
                case 117:
                    __pushoperator("negate","J");
                    break;
                // fneg
                case 118:
                    __pushoperator("negate","F");
                    break;
                // dneg
                case 119:
                    __pushoperator("negate","D");
                    break;
                // ishl
                case 120:
                    __pushbioperator("<<","I");
                    break;
                // lshl
                case 121:
                    __pushbioperator("<<","J");
                    break;
                // ishr
                case 122:
                    __pushbioperator(">>","I");
                    break;
                // lshr
                case 123:
                    __pushbioperator(">>","J");
                    break;
                // iushr
                case 124:
                    __pushbioperator(">>>","I");
                    break;
                // lushr
                case 125:
                    __pushbioperator(">>>","J");
                    break;
                // iand
                case 126:
                    __pushbioperator("&","I");
                    break;
                // land
                case 127:
                    __pushbioperator("&","J");
                    break;
                // ior
                case 128:
                    __pushbioperator("|","I");
                    break;
                // lor
                case 129:
                    __pushbioperator("|","J");
                    break;
                // ixor
                case 130:
                   __pushbioperator("^","I");
                    break;
                // lxor
                case 131:
                    __pushbioperator("^","J");
                    break;
                // iinc
                case 132:
                    if(wide) {
                        varindex = (ci.byteAt(index+1) << 8) | ci.byteAt(index+2);
                        offset = (ci.byteAt(index+3) << 8) | ci.byteAt(index+4);
                        wide = false;
                        ci.move(index+5);
                    }
                    else {
                        varindex = ci.byteAt(index+1);
                        //It converts a signed byte to an integer
                        offset = (int) (((byte) ci.byteAt(index+2)) & (byte) 0xFF) ;
                        //varindex = ci.byteAt(index+1);
                        ci.move(index+3);
                    }

                    // object list
                    obj = scala.collection.immutable.List.empty();
                    obj = obj.$colon$colon((Statement)
                            new Variable(
                                jpp,
                              //TODO: the variable identifier now requires a type.
                                new VariableIdentifier("val" + Integer.toString(varindex) + "#" + localtypes.get(varindex).getRevision(), new JavaType("TODO"), jpp)
                            )
                    );

                    // parameter list
                    param = scala.collection.immutable.List.empty();
                    param = param.$colon$colon((Statement)
                            new NumericalConstant(
                                jpp,
                                Integer.toString(offset),
                                new JavaType("I")
                            )
                    );

                    __flushids(
                            (Statement)
                            new MethodCall(
                                jpp,
                                new FieldAccess (
                                    jpp,
                                    obj,
                                    "+",
                                    new JavaType("(I)V")
                                ),
                                null,
                                param,
                                new JavaType("V")
                            ),
                            false);
                    break;
                // i2l
                case 133:
                    __pushoperator("i2l","J");
                    break;
                // i2f
                case 134:
                    __pushoperator("i2f","F");
                    break;
                // i2d
                case 135:
                    __pushoperator("i2d","D");
                    break;
                // l2i
                case 136:
                    __pushoperator("l2i","I");
                    break;
                // l2f
                case 137:
                    __pushoperator("l2f","F");
                    break;
                // l2d
                case 138:
                    __pushoperator("l2d","D");
                    break;
                // f2i
                case 139:
                    __pushoperator("f2i","I");
                    break;
                // f2l
                case 140:
                    __pushoperator("f2l","J");
                    break;
                // f2d
                case 141:
                    __pushoperator("f2d","D");
                    break;
                // d2i
                case 142:
                    __pushoperator("d2i","I");
                    break;
                // d2l
                case 143:
                    __pushoperator("d2l","J");
                    break;
                // d2f
                case 144:
                    __pushoperator("d2f","F");
                    break;
                // i2b
                case 145:
                    __pushoperator("i2b","I");
                    break;
                // i2c
                case 146:
                    __pushoperator("i2c","I");
                    break;
                // i2s
                case 147:
                    __pushoperator("i2s","I");
                    break;
                // lcmp
                case 148:
                    __pushbioperator("compare", "J", "I");
                    break;
                // fcmpl
                case 149:
                    __pushbioperator("comparel", "F", "I");
                    break;
                // fcmpg
                case 150:
                    __pushbioperator("compareg", "F", "I");
                    break;
                // dcmpl
                case 151:
                    __pushbioperator("comparel", "D", "I");
                    break;
                // dcmpg
                case 152:
                    __pushbioperator("comapreg", "D", "I");
                    break;
                // ifeq
                case 153:
                    __comparisonzero("==");
                    break;
                // ifne
                case 154:
                    __comparisonzero("!=");
                    break;
                // iflt
                case 155:
                    __comparisonzero("<");
                    break;
                // ifge
                case 156:
                    __comparisonzero(">=");
                    break;
                // ifgt
                case 157:
                    __comparisonzero(">");
                    break;
                // ifle
                case 158:
                    __comparisonzero("<=");
                    break;
                // if_icmpeq
                case 159:
                   __comparison("==", "I", "Z");
                    break;
                // if_icmpne
                case 160:
                    __comparison("!=", "I", "Z");
                    break;
                // if_icmplt
                case 161:
                    __comparison("<", "I", "Z");
                    break;
                // if_icmpge
                case 162:
                    __comparison(">=", "I", "Z");
                    break;
                // if_icmpgt
                case 163:
                    __comparison(">", "I", "Z");
                    break;
                // if_icmple
                case 164:
                    __comparison("<=", "I", "Z");
                    break;
                // if_acmpeq
                case 165:
                    __comparison("==", "Ljava/lang/Object;", "Z");
                    break;
                // if acmpne
                case 166:
                    __comparison("!=", "Ljava/lang/Object;", "Z");
                    break;
                // goto
                case 167:
                    __flushids((Statement)new EmptyStatement(jpp),false);
                    unconditionaljumps.put(__getactualindex(index), __getactualindex(index + ((short)((ci.byteAt(index+1) << 8) | ci.byteAt(index+2)))));
                    break;
                // jsr
                case 168:
                    // store origin and destination
                    offset = (ci.byteAt(index+1) << 8) | ci.byteAt(index+2);
                    jsrcalls.push("JSR"+Integer.toString(index)+":"+Integer.toString(offset+index));

                    // push return address to op stack
                    __pushconstant(Integer.toString(index), "I");

                    // continue parsing at destination
                    ci.move(offset+index);
                    break;
                // ret
                case 169:
                    // retrieve origin
                    String jsrorigin = (String)jsrcalls.pop();
                    offset = Integer.valueOf(jsrorigin.substring(3,jsrorigin.indexOf(":")));

                    // continue parsing at origin
                    ci.move(offset);
                    ci.next();
                    break;
                // tableswitch
                case 170:
                    __tableswitch();
                    break;
                // lookupswitch
                case 171:
                    __lookupswitch();
                    break;
                // ireturn
                case 172:
                // lreturn
                case 173:
                // freturn
                case 174:
                // dreturn
                case 175:
                // areturn
                case 176:
                    __flushids(((TypedStatement)opsta.pop()).getStatement(),false);
                    break;
                // return
                case 177:
                    __flushids((Statement)new EmptyStatement(jpp),false);
                    break;
                // getstatic
                case 178:
                    __readfield((ci.byteAt(index+1) << 8) | ci.byteAt(index+2), true);
                    break;
                // putstatic
                case 179:
                    __writefield((ci.byteAt(index+1) << 8) | ci.byteAt(index+2), true);
                    break;
                // getfield
                case 180:
                    __readfield((ci.byteAt(index+1) << 8) | ci.byteAt(index+2), false);
                    break;
                // putfield
                case 181:
                    __writefield((ci.byteAt(index+1) << 8) | ci.byteAt(index+2), false);
                    break;
                // invokevirtual
                case 182:
                // invokespecial
                case 183:
                    __invokemethod((ci.byteAt(index+1) << 8) | ci.byteAt(index+2), false, false);
                    break;
                    // invokestatic
                case 184:
                    __invokemethod((ci.byteAt(index+1) << 8) | ci.byteAt(index+2), true, false);
                    break;
                // invokeinterface
                case 185:
                    __invokemethod((ci.byteAt(index+1) << 8) | ci.byteAt(index+2), false, true);
                    break;
                // new
                case 187:
                    offset = (ci.byteAt(index+1) << 8) | ci.byteAt(index+2);
                    statype = new JavaType(te.objectType(cp.getClassInfo(offset)));
                    opsta.push(
                            new TypedStatement(
                                new New(jpp, statype),
                                statype
                            )
                    );
                    break;
                // newarray
                case 188:
                    switch(ci.byteAt(index+1)) {
                        case 4:
                            statype = new JavaType("[Z");
                            break;
                        case 5:
                            statype = new JavaType("[C");
                            break;
                        case 6:
                            statype = new JavaType("[F");
                            break;
                        case 7:
                            statype = new JavaType("[D");
                            break;
                        case 8:
                            statype = new JavaType("[B");
                            break;
                        case 9:
                            statype = new JavaType("[S");
                            break;
                        case 10:
                            statype = new JavaType("[I");
                            break;
                        case 11:
                            statype = new JavaType("[J");
                            break;
                    }
                    // object list
                    obj = scala.collection.immutable.List.empty();
                    obj = obj.$colon$colon((Statement)new New(jpp, statype));

                    // parameter list
                    param = scala.collection.immutable.List.empty();
                    param = param.$colon$colon(((TypedStatement)opsta.pop()).getStatement());

                    // NOTE: Used MethodCall to initialize as a constructor
                    opsta.push(
                        new TypedStatement(
                            new MethodCall(
                                jpp,
                                new FieldAccess(
                                    jpp,
                                    obj,
                                    "initialize",
                                    new JavaType("(I)"+statype.getName())
                                ),
                                null,
                                param,
                                statype
                            ),
                            statype
                        )
                    );
                    break;
                // anewarray
                case 189:
                    offset = (ci.byteAt(index+1) << 8) | ci.byteAt(index+2);
                    statype = new JavaType("[" + te.objectType(cp.getClassInfo(offset)));

                    // object list
                    obj = scala.collection.immutable.List.empty();
                    obj = obj.$colon$colon((Statement)new New(jpp, statype));

                    // parameter list
                    param = scala.collection.immutable.List.empty();
                    param = param.$colon$colon(((TypedStatement)opsta.pop()).getStatement());

                    // BIGNOTE: Used MethodCall to initialize as a constructor
                    opsta.push(
                        new TypedStatement(
                            new MethodCall(
                                jpp,
                                new FieldAccess(
                                    jpp,
                                    obj,
                                    "initialize",
                                    new JavaType("(I)"+statype.getName())
                                ),
                                null,
                                param,
                                statype
                            ),
                            statype
                        )
                    );
                    break;
                // arraylength
                case 190:
                    op1 = (TypedStatement)opsta.pop();

                    // object list
                    obj = scala.collection.immutable.List.empty();
                    obj = obj.$colon$colon(op1.getStatement());

                    statype = new JavaType("I");
                    opsta.push(
                            new TypedStatement(
                                new MethodCall(
                                        jpp,
                                        new FieldAccess (
                                            jpp,
                                            obj,
                                            "length",
                                            new JavaType("()I")
                                        ),
                                        null,
                                        null,
                                        statype
                                ),
                                statype
                             )
                    );
                    break;
                // athrow
                case 191:
                    op1 = (TypedStatement)opsta.pop();
                    opsta.clear();
                    opsta.push(op1);

                    __flushids((Statement)new Throw(jpp, op1.getStatement()),false);
                    break;
                // checkcast
                case 192:
                    offset = (ci.byteAt(index+1) << 8) | ci.byteAt(index+2);
                    op1 = (TypedStatement)opsta.peek();

                    // object list
                    obj = scala.collection.immutable.List.empty();
                    obj = obj.$colon$colon(op1.getStatement());

                    // parameter list
                    param = scala.collection.immutable.List.empty();
                    param = param.$colon$colon((Statement)new NumericalConstant(jpp, te.objectType(cp.getClassInfo(offset)), new JavaType("Ljava/lang/String;")));

                    __flushids(
                            (Statement)new MethodCall(
                                        jpp,
                                        new FieldAccess (
                                            jpp,
                                            obj,
                                            "isInstanceOf",
                                            new JavaType("(Ljava/lang/String;)Z")
                                        ),
                                        null,
                                        param,
                                        new JavaType("Z")
                                ),
                                false);
                    break;
                // instanceof
                case 193:
                    offset = (ci.byteAt(index+1) << 8) | ci.byteAt(index+2);
                    op1 = (TypedStatement)opsta.pop();

                    // object list
                    obj = scala.collection.immutable.List.empty();
                    obj = obj.$colon$colon(op1.getStatement());

                    // parameter list
                    param = scala.collection.immutable.List.empty();
                    param = param.$colon$colon((Statement)new NumericalConstant(jpp, te.objectType(cp.getClassInfo(offset)), new JavaType("Ljava/lang/String;")));

                    statype = new JavaType("I");
                    opsta.push(
                            new TypedStatement(
                                new MethodCall(
                                        jpp,
                                        new FieldAccess (
                                            jpp,
                                            obj,
                                            "isInstanceOf",
                                            new JavaType("(Ljava/lang/String;)Z")
                                        ),
                                        null,
                                        param,
                                        statype
                                ),
                                statype
                            )
                     );
                    break;
                // monitorenter
                case 194:
                // monitorexit
                case 195:
                    String monitorfun = "monitorexit";
                    if(op == 104)
                        monitorfun = "monitorenter";

                    op1 = (TypedStatement)opsta.pop();

                    // object list
                    obj = scala.collection.immutable.List.empty();
                    obj = obj.$colon$colon(op1.getStatement());

                    __flushids((Statement)new MethodCall(
                                        jpp,
                                        new FieldAccess (
                                            jpp,
                                            obj,
                                            monitorfun,
                                            new JavaType("()V")
                                        ),
                                        null,
                                        null,
                                        new JavaType("V")
                                ),
                                false);
                    break;
                // wide
                case 196:
                    wide = true;
                    ci.move(index+1);
                    break;
                // multianewarray
                case 197:
                    offset = (ci.byteAt(index+1) << 8) | ci.byteAt(index+2);
                    statype = new JavaType(cp.getClassInfo(offset));
                    int dimensions = ci.byteAt(index+3);

                    // object list
                    obj = scala.collection.immutable.List.empty();
                    obj = obj.$colon$colon((Statement)new New(jpp, statype));

                    // parameter list
                    param = scala.collection.immutable.List.empty();
                    String methodtype = "(";

                    for(int i = 0; i<dimensions; i++) {
                        param = param.$colon$colon(((TypedStatement)opsta.pop()).getStatement());
                        methodtype = methodtype + "I";
                    }

                    methodtype = methodtype + ")" + cp.getClassInfo(offset);

                    // BIGNOTE: Used MethodCall to initialize as a constructor
                    opsta.push(
                        new TypedStatement(
                            new MethodCall(
                                jpp,
                                new FieldAccess(
                                    jpp,
                                    obj,
                                    "initialize",
                                    new JavaType(methodtype)
                                ),
                                null,
                                param,
                                statype
                            ),
                            statype
                        )
                    );
                    break;
                // ifnull
                case 198:
                // ifnonnull
                case 199:
                    op1 = (TypedStatement)opsta.pop();
                    String functionname = "!=";
                    if(op==198)
                        functionname = "==";
                    // object list
                    obj = scala.collection.immutable.List.empty();
                    obj = obj.$colon$colon(op1.getStatement());

                    // BIGNOTE: what is the correct type of null?
                    // parameter list
                    param = scala.collection.immutable.List.empty();
                    param = param.$colon$colon((Statement)new NumericalConstant(jpp, "null", new JavaType("Ljava/lang/Object;")));

                    __flushids(
                            (Statement)
                            new MethodCall(
                                jpp,
                                new FieldAccess (
                                    jpp,
                                    obj,
                                    functionname,
                                    new JavaType("(Ljava/lang/Object;)Z")
                                ),
                                null,
                                param,
                                new JavaType("Z")
                            ),
                            true);
                    break;
                // goto_w
                case 200:
                    __flushids((Statement)new EmptyStatement(jpp),false);
                    unconditionaljumps.put(__getactualindex(index), __getactualindex(index + ((ci.byteAt(index+1) << 24) | (ci.byteAt(index+2) << 16) | (ci.byteAt(index+3) << 8) | (ci.byteAt(index+4)))));
                    break;
                // jsr_w
                case 201:
                    // store origin and destination
                    offset = (ci.byteAt(index+1) << 24) | (ci.byteAt(index+2) << 16) | (ci.byteAt(index+3) << 8) | ci.byteAt(index+4);
                    jsrcalls.push("JSR"+Integer.toString(index)+":"+Integer.toString(offset+index));

                    // continue parsing at destination
                    ci.move(offset+index);
                    break;
            }
        }

        // build the cfg out of the parsed statement list
        __buildcfg();
        return cfg;
    }

    /**
     * Pushes a NumericalConstant on the operand stack
     *
     * @param value
     * @param type
     */
    private void __pushconstant(String value, String type) {
        JavaType statype = new JavaType(type);
        opsta.push(new TypedStatement(new NumericalConstant(
                                                jpp,
                                                value,
                                                statype
                                      ),
                                      statype)
                  );
    }

    /**
     * Pushes a Variable read on the operand stack
     * @param number Which variable?
     */
    private void __pushvariable(int number) {
        JavaType statype = localtypes.get(number).getJavaType();
        opsta.push(new TypedStatement(new Variable(
                                            jpp,
                                            //TODO: the variable identifier now requires a type.
                                            new VariableIdentifier("val" + Integer.toString(number) + "#" + localtypes.get(number).getRevision(), new JavaType("TODO"), jpp)
                                      ),
                                      statype)
                  );
    }

    /**
     * Adds a VariableAssignment to list of parsed statements
     * @param number Assignment to which Variable
     */
    private void __variableassignment(int number) {
        TypedStatement op1 = ((TypedStatement)opsta.pop());

        Statement s1;
        // assignment
        if(localtypes.get(number) != null && localtypes.get(number).getType().equals(op1.getType().getName())) {
                s1 = new Assignment(
                        jpp,
                      //TODO: the variable identifier now requires a type.
                        new Variable(jpp, new VariableIdentifier("val"+number+"#" + localtypes.get(number).getRevision(), new JavaType("TODO"), jpp)),
                        op1.getStatement()
                );
        }
        // declaration or redeclaration
        else {
            if(localtypes.get(number) == null) {
                localtypes.set(number, new LocalVariableType(op1.getType().getName()));
            }
            else if(!localtypes.get(number).getType().equals(op1.getType().getName())) {
                localtypes.get(number).setType(op1.getType().getName());
            }
            s1 = new VariableDeclaration(
                        jpp,
                        new Variable(
                            jpp,
                            //TODO: the variable identifier now requires a type.
                            new VariableIdentifier("val"+number+"#" + localtypes.get(number).getRevision(), new JavaType("TODO"), jpp)
                        ),
                        localtypes.get(number).getJavaType(),
                        op1.getStatement()
                    );
        }
        __flushids(s1,false);
    }

    /**
     * Pushes a read access to an array on the operand stack
     * @param type
     */
    private void __pusharrayaccessread(String type) {
        TypedStatement op2 = (TypedStatement)opsta.pop();
        TypedStatement op1 = (TypedStatement)opsta.pop();

        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        obj = obj.$colon$colon(op1.getStatement());

        scala.collection.immutable.List<Statement> param = scala.collection.immutable.List.empty();
        param = param.$colon$colon(op2.getStatement());

        JavaType statype = new JavaType(type);
        opsta.push(new TypedStatement(new MethodCall( jpp,
                                                      new FieldAccess(
                                                        jpp,
                                                        obj,
                                                        "apply",
                                                        new JavaType("(I)"+type)
                                                      ),
                                                      null,
                                                      param,
                                                      statype
                                          ),
                                          statype
                                  )
                    );
    }

    /**
     * Adds an array write access to list of parsed statements
     * @param type
     */
    private void __pusharrayaccesswrite(String type) {
        TypedStatement op3 = ((TypedStatement)opsta.pop());
        TypedStatement op2 = ((TypedStatement)opsta.pop());
        TypedStatement op1 = ((TypedStatement)opsta.pop());

        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        obj = obj.$colon$colon(op1.getStatement());

        // parameter list: index and value
        scala.collection.immutable.List<Statement> param = scala.collection.immutable.List.empty();
        param = param.$colon$colon(op2.getStatement());
        param = param.$colon$colon(op3.getStatement());

        __flushids(
                (Statement)new MethodCall(
                                                jpp,
                                                new FieldAccess(
                                                    jpp,
                                                    obj,
                                                    "update",
                                                    new JavaType("(I"+type+")V")
                                                ),
                                                null,
                                                param,
                                                new JavaType("V")
                                             ),
                                             false);
    }

    /**
     * Pushes an operation to the operand stack, which is applied to two
     * operands.
     *
     * @param operationtype +.-,* etc...
     * @param type of argument
     * @param returntype
     */
    private void __pushbioperator(String operationtype, String type, String returntype) {
        TypedStatement op2 = (TypedStatement)opsta.pop();
        TypedStatement op1 = (TypedStatement)opsta.pop();

        // object list
        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        obj = obj.$colon$colon(op1.getStatement());

        // parameter list
        scala.collection.immutable.List<Statement> param = scala.collection.immutable.List.empty();
        param = param.$colon$colon(op2.getStatement());

        JavaType statype = new JavaType(returntype);
        opsta.push(
                new TypedStatement(
                    new MethodCall(
                            jpp,
                            new FieldAccess (
                                jpp,
                                obj,
                                operationtype,
                                new JavaType("("+type+")"+returntype)
                            ),
                            null,
                            param,
                            statype
                        ),
                        statype
                        )
                        );
    }
    /**
     * Pushes an operation to the operand stack, which is applied to two
     * operands.
     *
     * @param operationtype +.-,* etc...
     * @param type
     */
    private void __pushbioperator(String operationtype, String type) {
        TypedStatement op2 = (TypedStatement)opsta.pop();
        TypedStatement op1 = (TypedStatement)opsta.pop();

        // object list
        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        obj = obj.$colon$colon(op1.getStatement());

        // parameter list
        scala.collection.immutable.List<Statement> param = scala.collection.immutable.List.empty();
        param = param.$colon$colon(op2.getStatement());

        JavaType statype = new JavaType(type);
        opsta.push(
                new TypedStatement(
                    new MethodCall(
                            jpp,
                            new FieldAccess (
                                jpp,
                                obj,
                                operationtype,
                                new JavaType("("+type+")"+type)
                            ),
                            null,
                            param,
                            statype
                        ),
                        statype
                        )
                        );
    }

    /**
     * Pushes an operation to the operand stack, wehich is applied to just one
     * operand (e.g. conversions)
     *
     * @param operationtype
     * @param returntype
     */
    private void __pushoperator(String operationtype, String returntype) {
        TypedStatement op1 = (TypedStatement)opsta.pop();

        // object list
        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        obj = obj.$colon$colon(op1.getStatement());

        JavaType statype = new JavaType(returntype);
        JavaType mcall = new JavaType("()"+returntype);

        opsta.push(
                new TypedStatement(
                    new MethodCall(
                            jpp,
                            new FieldAccess (
                                jpp,
                                obj,
                                operationtype,
                                mcall
                            ),
                            null,
                            null,
                            statype
                    ),
                    statype
                )
        );
    }

    /**
     * Adds a comparison of one operand with zero to the list of parsed
     * statements.
     * @param comptype (>,<,==, etc.)
     */
    private void __comparisonzero(String comptype) {
        TypedStatement op1 = (TypedStatement)opsta.pop();

        // object list
        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        obj = obj.$colon$colon(op1.getStatement());

        // parameter list
        scala.collection.immutable.List<Statement> param = scala.collection.immutable.List.empty();
        param = param.$colon$colon((Statement)new NumericalConstant(jpp, Integer.toString(0), new JavaType("I")));

        __flushids(
                (Statement)
                                new MethodCall(
                                jpp,
                                new FieldAccess (
                                    jpp,
                                    obj,
                                    comptype,
                                    new JavaType("(I)Z")
                                ),
                                null,
                                param,
                                new JavaType("Z")
                            ),
                            true);
    }

    /**
     * Adds a comparison of one operand with another to the list of parsed
     * statements.
     * @param comptype (>,<,==, etc.)
     * @param type of compared objects
     * @param returntype
     */
    private void __comparison(String comptype, String type, String returntype) {
        TypedStatement op2 = (TypedStatement)opsta.pop();
        TypedStatement op1 = (TypedStatement)opsta.pop();

        // object list
        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        obj = obj.$colon$colon(op1.getStatement());

        // parameter list
        scala.collection.immutable.List<Statement> param = scala.collection.immutable.List.empty();
        param = param.$colon$colon(op2.getStatement());

        __flushids(
                (Statement)
                new MethodCall(
                    jpp,
                    new FieldAccess (
                        jpp,
                        obj,
                        comptype,
                        new JavaType("("+type+")"+returntype)
                    ),
                    null,
                    param,
                    new JavaType(returntype)
                ),
                true);
    }

    /**
     * Pushes a read FieldAccess to the operand stack
     *
     * @param offset into the constan pool describing the field
     * @param isstatic do we have a static field?
     */
    private void __readfield(int offset, boolean isstatic) {
        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();

        if(isstatic)
        	//TODO: the variable identifier now requires a type.
            obj = obj.$colon$colon((Statement)new Variable(jpp, new VariableIdentifier(cp.getFieldrefClassName(offset), new JavaType("TODO"), jpp)));
        else {
            TypedStatement op1 = (TypedStatement)opsta.pop();
            obj = obj.$colon$colon(op1.getStatement());
        }

        JavaType statype = new JavaType(cp.getFieldrefType(offset));
        opsta.push(
                new TypedStatement(
                    new FieldAccess(
                            jpp,
                            obj,
                            cp.getFieldrefName(offset),
                            statype
                    ),
                    statype
                )
        );

    }

     /**
     * Adds Assignment to a FieldAccess to the list of parsed statements.
     *
     * @param offset into the constan pool describing the field
     * @param isstatic do we have a static field?
     */
    private void __writefield(int offset, boolean isstatic) {
        TypedStatement op2 = (TypedStatement)opsta.pop();

        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        if(isstatic)
        	//TODO: the variable identifier now requires a type.
            obj = obj.$colon$colon((Statement)new Variable(jpp, new VariableIdentifier(cp.getFieldrefClassName(offset), new JavaType("TODO"), jpp)));
        else {
            TypedStatement op1 = (TypedStatement)opsta.pop();
            obj = obj.$colon$colon(op1.getStatement());
        }

        __flushids(
                (Statement)
                new Assignment(
                        jpp,
                        new FieldAccess(jpp, obj, cp.getFieldrefName(offset), new JavaType(cp.getFieldrefType(offset))),
                        op2.getStatement()
                ),
                false);
    }

    /**
     * Adds an Assignment of the result of MethodCall to a newly created Variable
     * to the list of parsed statements. The temporarily created variable
     * holding the result of the MethodCall to the is then pushed to the
     * operand stack. The introduction of a variable is used to cope with
     * possible side effects of methods.
     *
     * @param offset into the constant pool describing the method
     * @param isstatic is it a static method?
     * @param isinterface is it an interface?
     */
    private void __invokemethod(int offset, boolean isstatic, boolean isinterface) {
        // poping all parameters
        String classname = null;
        String methodname = null;
        String methodtype = null;
        if(isinterface) {
            te.setDescriptor(cp.getInterfaceMethodrefType(offset));
            classname = cp.getInterfaceMethodrefClassName(offset);
            methodname=cp.getInterfaceMethodrefName(offset);
            methodtype=cp.getInterfaceMethodrefType(offset);
        }
        else {
            te.setDescriptor(cp.getMethodrefType(offset));
            classname = cp.getMethodrefClassName(offset);
            methodname=cp.getMethodrefName(offset);
            methodtype=cp.getMethodrefType(offset);
        }
        scala.collection.immutable.List<Statement> param = scala.collection.immutable.List.empty();
        for(int i = 0; i < te.getParameterCount(); i++) {
            param = param.$colon$colon(((TypedStatement)opsta.pop()).getStatement());
        }
        // object list
        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        if(isstatic)
        	//TODO: the variable identifier now requires a type.
            obj = obj.$colon$colon((Statement)new Variable(jpp, new VariableIdentifier(classname, new JavaType("TODO"), jpp)));
        else{
            TypedStatement op1 = (TypedStatement)opsta.pop();
            obj = obj.$colon$colon(op1.getStatement());
        }
        
        // introduce a temporary variable to cope with side effects of functions
        tempfunctionvariable++;
        JavaType statype = new JavaType(te.getReturnType());
        opsta.push(
                new TypedStatement(
                	//TODO: the variable identifier now requires a type.
                    new Variable(jpp, new VariableIdentifier("#tempfun" + classname + Integer.toString(tempfunctionvariable), new JavaType("TODO"), jpp)),
                    statype
                )
        );
        __flushids(
                new Assignment( jpp,
                						//TODO: The variable identifier now requires also a type. 
                                        new Variable(jpp, new VariableIdentifier("#tempfun" + classname + Integer.toString(tempfunctionvariable), new JavaType("TODO"), jpp)),
                                        new MethodCall(
                                            jpp,
                                            new FieldAccess (
                                                jpp,
                                                obj,
                                                methodname,
                                                new JavaType(methodtype)
                                            ),
                                            null,
                                            param,
                                            statype
                                        )
                         ),
                         false);
    }

    /**
     * Translates the tableswitch opcode to a series of if(index == exp1) goto
     * dest1 expressions. Each of the comparisons is added to the list of parsed
     * statements and corresponding jumps are added.
     */
    private void __tableswitch() {
        // determine padding
        int offset = (4 - (index % 4));

        // determine default, low and high and the number of jump entries
        int defaultv = ci.s32bitAt(index + offset);
        int lowv = ci.s32bitAt(index + offset+4);
        int highv = ci.s32bitAt(index + offset+8);
        int count = highv - lowv + 1;

        // extract index
        TypedStatement op1 = (TypedStatement)opsta.pop();

        // object list
        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        obj = obj.$colon$colon(op1.getStatement());

        // parse each entry
        int jmpid = -1;
        for(int i = 0; i < count; i++) {
            // extract where to jump
            jmpid = index + offset + 12 + i*4;
            int address = ci.s32bitAt(jmpid);

            // we actually generate virtual statements
            ins.push(__getactualindex(jmpid));

            // parameter list
            scala.collection.immutable.List<Statement> param = scala.collection.immutable.List.empty();
            param = param.$colon$colon((Statement)new NumericalConstant(jpp, Integer.toString(jmpid), new JavaType("I")));

            Statement st = (Statement)
                                new MethodCall(
                                jpp,
                                new FieldAccess (
                                    jpp,
                                    obj,
                                    "==",
                                    new JavaType("(I)Z")
                                ),
                                null,
                                param,
                                new JavaType("Z")
                            );

            parsedStatements.add(st);


            staidtoins.put(parsedStatements.indexOf(st), (Stack)ins.clone());
            while(!ins.isEmpty())
                instostaid.put((String)ins.pop(), parsedStatements.indexOf(st));

            falsejumps.put(__getactualindex(jmpid), __getactualindex(jmpid+4));
            truejumps.put(__getactualindex(jmpid), __getactualindex(index+address));
        }
        // adjust last else case
        falsejumps.remove(falsejumps.get(__getactualindex(jmpid)));
        falsejumps.put(__getactualindex(jmpid),__getactualindex(index+defaultv));

    }

    /**
     * Translates the lookupswitch opcode to a series of if(key == exp1) goto
     * dest1 expressions. Each of the comparisons is added to the list of parsed
     * statements and corresponding jumps are added.
     */
    private void __lookupswitch() {
        // determine padding
        int offset = (4 - (index % 4));

        // determine default and npairs
        int defaultv = ci.s32bitAt(index + offset);
        int npairsv = ci.s32bitAt(index + offset+4);

        // extract key
        TypedStatement op1 = (TypedStatement)opsta.pop();

        // object list
        scala.collection.immutable.List<Statement> obj = scala.collection.immutable.List.empty();
        obj = obj.$colon$colon(op1.getStatement());

        // parse each entry
        int jmpid = -1;
        if(npairsv > 0) {
            for(int i = 0; i < npairsv; i++) {
                // extract where to jump
                jmpid = index + offset + 8 + i*8;
                int match = ci.s32bitAt(jmpid);
                int address = ci.s32bitAt(jmpid+4);

                // we actually generate virtual statements
                ins.push(__getactualindex(jmpid));

                // parameter list
                scala.collection.immutable.List<Statement> param = scala.collection.immutable.List.empty();
                param = param.$colon$colon((Statement)new NumericalConstant(jpp, Integer.toString(match), new JavaType("I")));

                Statement st = (Statement)
                                    new MethodCall(
                                    jpp,
                                    new FieldAccess (
                                        jpp,
                                        obj,
                                        "==",
                                        new JavaType("(I)Z")
                                    ),
                                    null,
                                    param,
                                    new JavaType("Z")
                                );

                parsedStatements.add(st);


                staidtoins.put(parsedStatements.indexOf(st), (Stack)ins.clone());
                while(!ins.isEmpty())
                    instostaid.put((String)ins.pop(), parsedStatements.indexOf(st));

                falsejumps.put(__getactualindex(jmpid), __getactualindex(jmpid+8));
                truejumps.put(__getactualindex(jmpid), __getactualindex(index+address));
            }
            // adjust last else case
            falsejumps.remove(falsejumps.get(__getactualindex(jmpid)));
            falsejumps.put(__getactualindex(jmpid),__getactualindex(index+defaultv));
        }
        else {
            __flushids((Statement)new EmptyStatement(jpp),false);
            falsejumps.put(__getactualindex(index),__getactualindex(index+defaultv));
        }

    }

    /**
     * Given an integer index this function returns a string representation of
     * the current opcode address. This is actually used to distinguish between
     * normaly parsed statements and 'JSR'-parsed statements.
     *
     * In the normal case the index is just returned as a string. In the JSR-
     * case a string like "JSRfrom:to" is appended to the index.
     *
     * @param index
     * @return
     */
    private String __getactualindex(int index) {
        // are we actually parsing a JSR or not
        if(jsrcalls.empty())
            return Integer.toString(index);
        else
            return Integer.toString(index) + (String)jsrcalls.peek();
    }

    /**
     * Adds the given Statement to the list of parsed statements and stores the
     * mapping between the bytecode addresses and statement ids. If necessary
     * conditional jumps are added.
     *
     * @param st The statement to add
     * @param conditional Switch for adding conditional jumps
     */
    private void __flushids(Statement st, boolean conditional) {
        // add the given statemtn to the list of parsed statements
        parsedStatements.add(st);
        staid = parsedStatements.indexOf(st);

        // remember the mappings metween bytcode addresses and statement ids
        staidtoins.put(staid, (Stack)ins.clone());
        while(!ins.isEmpty())
            instostaid.put((String)ins.pop(), staid);

        // add jumps if necessary
        if(conditional)
        {
            falsejumps.put(__getactualindex(index), __getactualindex(ci.lookAhead()));
            truejumps.put(__getactualindex(index), __getactualindex(index + (short)(((ci.byteAt(index+1) << 8) | ci.byteAt(index+2)))));
        }

        //System.out.println("* bytcode instructions flushed to statement with id: \t" + staid);
    }

    /**
     * Build the ControlFlowGraph for the method currently beeing parsed and
     * stores it in private variable cfg. Code blocks have to be as large as
     * possible but still supporting all jumps.
     */
    private void __buildcfg() {
        Stack staids = new Stack();

        // map for remembering which statement id was mapped to which block/node
        Map<Integer, Integer> statonode = new HashMap();

        // maps for newly inserted jumps between blocks
        Map<Integer, Integer> insertedjumps = new HashMap();

        // Scalalist for building the block/node
        scala.collection.immutable.List<Statement> stalist = scala.collection.immutable.List$.MODULE$.empty();
scala.collection.immutable.List.empty();

        int insjmpid = -1;

        // iterate through the parsedStatements
        for(Iterator li = parsedStatements.iterator(); li.hasNext(); ) {
            Statement st = (Statement)li.next();
            int stid = parsedStatements.indexOf(st);

            // we have to insert a jump between blocks
            if(insjmpid > 0) {
                insertedjumps.put(insjmpid, stid);
                insjmpid = -1;
            }

            boolean target = false;
            boolean source = false;
            // determine wheather the current statement t is the source or the target of a jump
            for(Iterator i = staidtoins.get(stid).iterator(); i.hasNext();) {
                String a = (String)i.next();
                if(unconditionaljumps.containsValue(a) || falsejumps.containsValue(a)|| truejumps.containsValue(a) )
                    target = true;
                if(unconditionaljumps.containsKey(a) || falsejumps.containsKey(a) || truejumps.containsKey(a))
                    source = true;
            }
            /*
            // if target: immediatly begin new block, and then add current statement
            // add unconditional jump between blocks
            if(target) {
                int nodeid = cfg.addNode(stalist);
                while(!staids.isEmpty())
                    statonode.put((Integer)staids.pop(), nodeid);
                stalist = (new listhelperlib.ScalaListHelperClass<Statement>()).getEmptyList();
                stalist = stalist.$colon$colon(st);
                staids.push(stid);
                insjmpid = stid;
            }
            // if source: add current statement and then finish block
            else if(source) {
                stalist = stalist.$colon$colon(st);
                staids.push(stid);
                int nodeid = cfg.addNode(stalist);

                while(!staids.isEmpty())
                    statonode.put((Integer)staids.pop(), nodeid);
                stalist = (new listhelperlib.ScalaListHelperClass<Statement>()).getEmptyList();
            }
            // just add the current statement to the current block
            else {
                stalist = stalist.$colon$colon(st);
                staids.push(stid);
            }
            */

	         // if target: immediatly begin new block
	         // add unconditional jump between blocks
	         if(target) {
	            insjmpid = -2;
	            int nodeid = cfg.addNode(stalist);
	            while(!staids.isEmpty())
	                statonode.put((Integer)staids.pop(), nodeid);
	            stalist = scala.collection.immutable.List.empty();
	         }
	         // just add the current statement to the current block
	         stalist = stalist.$colon$colon(st);
	         staids.push(stid);
	         if(insjmpid == -2)
	           insjmpid = stid;
	         // if source: finish block
	         if(source) {
	            int nodeid = cfg.addNode(stalist);
	            while(!staids.isEmpty())
	                statonode.put((Integer)staids.pop(), nodeid);
	            stalist = scala.collection.immutable.List.empty();
	         }

        }

        // finish the last block if we began one
        if(!staids.isEmpty()) {
            int nodeid = cfg.addNode(stalist);
            while(!staids.isEmpty())
                statonode.put((Integer)staids.pop(), nodeid);
        }

        for(Map.Entry<String,String> edge : unconditionaljumps.entrySet()) {
            int from = statonode.get(instostaid.get(edge.getKey()));
            int to = statonode.get(instostaid.get(edge.getValue()));
            Option<Object> a = scala.Option.apply(null);
            cfg.addEdge(from, to, a);
        }
        for(Map.Entry<String,String> edge : truejumps.entrySet()) {
            int from = statonode.get(instostaid.get(edge.getKey()));
            int to = statonode.get(instostaid.get(edge.getValue()));
            cfg.addEdge(from, to, new scala.Some(new Boolean(true)));
        }
        for(Map.Entry<String,String> edge : falsejumps.entrySet()) {
            int from = statonode.get(instostaid.get(edge.getKey()));
            int to = statonode.get(instostaid.get(edge.getValue()));
            cfg.addEdge(from, to, new scala.Some(new Boolean(false)));
        }
        
        // build edges in the cfg
        for(Map.Entry<Integer,Integer> edge : insertedjumps.entrySet()) {
            int from = statonode.get(edge.getKey());
            int to = statonode.get(edge.getValue());
            Option<Object> a = scala.Option.apply(null);
            if(from!=to && cfg.getEdgesExitingFrom(from).size()==0) cfg.addEdge(from, to, a);
        }
        
    }
}
