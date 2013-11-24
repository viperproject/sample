package ch.ethz.inf.pm.sample.td.cost.loops;

import java.io.*;

public class PubsInterface {

    // used for testing only
    public static void main(String[] args) {
        String a = run("eq(cost(N),0,[l(N,0)],[]).\n" + "eq(l(N,I),1,[l(N,I+1)],[N>=I+1]).\n" + "eq(l(N,I),0,[],[I>=N]).");
        System.out.println(a);
    }

    public static String run(String input) {
        String s, result = null;
        try {
            // Create file
            FileWriter fstream = new FileWriter("pubs.ces");
            BufferedWriter out = new BufferedWriter(fstream);
            out.write(input);

            // Close the output stream
            out.close();

            // run PUBS
            Process p = Runtime.getRuntime().exec("./pubs_shell -file pubs.ces");
            BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));

            // read the output from the command line
            boolean correctEntry, done = false;
            while (!done && (s = stdInput.readLine()) != null) {
                if (s.contains("CRS $pubs_aux_entry$")) correctEntry = true;
                else if (s.contains("* Non Asymptotic Upper Bound:")) {
                    result = s.split(":")[1].trim();
                    done = true;
                }
            }
        }
        catch (IOException e) {
            System.out.println("exception happened: ");
            e.printStackTrace();
            System.exit(-1);
        }
        finally {
            if (result == null) result = "no upper bound found (error when using PUBS)";
        }
        return result;
    }

}
