package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain;

import apron.*;

public class ApronMemory {

    public static void  main (String args[]) {
        try {

            Octagon o = new Octagon();
            Environment eA = new Environment(new String[0], new String[0]);
            Abstract1 state = new Abstract1(o,eA);

            while (true) {
                state.changeEnvironment(o, eA, false);
            }

        } catch (apron.ApronException ex) {}
    }

}

//                i = i + 1;
//            int i = 0;
//            if (i%10000 == 0) {
//                System.out.println(i);
//                System.gc();
//                Runtime.getRuntime().runFinalization();
//            }


//state.assignCopy(o,b[0],new Texpr1Intern(e,new Texpr1CstNode(new apron.Interval(0,0))),null);
//state.foldCopy(o, b);
//Abstract1 x1 = state.meetCopy(o,state);
//Abstract1 x2 = state.joinCopy(o,state);
//Abstract1 x3 = state.unifyCopy(o,state);
//x1.meet(o,x2);
//x2.join(o,x3);
//x3.unify(o,x1);
//x1.meet(o,new Tcons1(Tcons1.EQ, new Texpr1Intern(e, new Texpr1CstNode(new DoubleScalar(0)))));
