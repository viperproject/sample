package ch.ethz.inf.pm.td.apron;

import apron.*;

/**
 * User: lucas
 * Date: 11/7/12
 * Time: 10:43 AM
 */
public class MeetBug {

    public static void  main(String[] a) {

        Manager man = new Octagon();

        String[] names = {"a", "b"};
        String[] empty = {};
        Environment env = new Environment(names, empty);

        /* =================================================================== */
        /* Creation of constraint a - b > 0 */
        /* =================================================================== */
        Texpr1Node exp1 =
                new Texpr1BinNode(Texpr1BinNode.OP_SUB,
                        new Texpr1VarNode("a"),
                        new Texpr1VarNode("b"));
        Tcons1 cons = new Tcons1(env, Tcons1.SUP, exp1);

        /* =================================================================== */
        /* Creation of polyhedra
           {  a >= 0;  -a +b >= 0;  a+b -1.0 >= 0;  b-1.0 >= 0 }  */
        /* =================================================================== */

        Linterm1[] t1 = { new Linterm1("a", new DoubleScalar(1)) };
        Linexpr1 e1 = new Linexpr1(env, t1, new DoubleScalar(0));
        Lincons1 c1 = new Lincons1( Lincons1.SUPEQ, e1);
        Linterm1[] t2 = { new Linterm1("a", new DoubleScalar(-1)), new Linterm1("b", new DoubleScalar(1)) };
        Linexpr1 e2 = new Linexpr1(env, t2, new DoubleScalar(0));
        Lincons1 c2 = new Lincons1( Lincons1.SUPEQ, e2);
        Linterm1[] t3 = { new Linterm1("a", new DoubleScalar(1)), new Linterm1("b", new DoubleScalar(1)) };
        Linexpr1 e3 = new Linexpr1(env, t3, new DoubleScalar(-1));
        Lincons1 c3 = new Lincons1( Lincons1.SUPEQ, e3);
        Linterm1[] t4 = { new Linterm1("b", new DoubleScalar(1)) };
        Linexpr1 e4 = new Linexpr1(env, t4, new DoubleScalar(-1));
        Lincons1 c4 = new Lincons1( Lincons1.SUPEQ, e4);

        Lincons1[] c = {c1,c2,c3,c4};

        System.out.println("lib: " + man.getLibrary());
        System.out.println("ver: " + man.getVersion());

        try {

            Abstract1 state = new Abstract1(man,c);

            System.out.println("Constraint: " + cons);
            System.out.println("State: " + state);

            System.out.println("Meet: " + state.meetCopy(man,cons));
            System.out.println("Double Meet: " + state.meetCopy(man,cons).meetCopy(man,cons));

        } catch (ApronException e) {
            e.printStackTrace();
        }

    }


}
