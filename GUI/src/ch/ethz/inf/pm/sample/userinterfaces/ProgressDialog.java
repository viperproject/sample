package ch.ethz.inf.pm.sample.userinterfaces;

import javax.swing.*;
import java.awt.event.*;

public class ProgressDialog extends JDialog {
    private JPanel contentPane;
    protected JTextPane PROGRESSOFTHEANALYSISTextPane;
    private JProgressBar progressBar1;

    public ProgressDialog() {
        setContentPane(contentPane);
        setModal(true);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        //setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    }


    public static void main(String[] args) {
        ProgressDialog dialog = new ProgressDialog();
        dialog.pack();
        dialog.setVisible(true);
        System.exit(0);
    }
}
