package ch.ethz.inf.pm.sample.userinterfaces;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class AnalysisResults extends JDialog {
    private JPanel contentPane;
    private JButton buttonOK;
	private JEditorPane resultPane;

    public AnalysisResults(String results) {
        setContentPane(contentPane);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onOK();
            }
        });

		results = results.replaceAll("\\n", "<br/>\n");
		results = results.replaceAll("Warning", "<font color=\"red\"><b>Warning</b></font>");
		results = results.replaceAll("Validated", "<b>Validated</b>");
        resultPane.setText(results);
    }

    private void onOK() {
        dispose();
    }
}
