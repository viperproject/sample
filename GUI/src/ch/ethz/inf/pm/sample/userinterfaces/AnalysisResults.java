package ch.ethz.inf.pm.sample.userinterfaces;

import ch.ethz.inf.pm.sample.SystemParameters;

import javax.swing.*;
import java.awt.event.*;

public class AnalysisResults extends JDialog {
    private JPanel contentPane;
    private JButton buttonOK;
    private JTextArea resultPane1;

    public AnalysisResults() {



        setContentPane(contentPane);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onOK();
            }
        });

        resultPane1.setText(SystemParameters.analysisOutput().getString());
        //setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

// call onCancel() when cross is clicked
        //setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    }

    private void onOK() {
// add your code here
        dispose();
    }

    public static void main(String[] args) {
        AnalysisResults dialog = new AnalysisResults();
        dialog.pack();
        dialog.setVisible(true);
        System.exit(0);
    }
}
