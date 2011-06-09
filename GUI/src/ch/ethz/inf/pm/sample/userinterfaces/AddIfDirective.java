package ch.ethz.inf.pm.sample.userinterfaces;

import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaProgramPoint;
import ch.ethz.inf.pm.sample.tracepartitioning.PartitionIf;
import ch.ethz.inf.pm.sample.tracepartitioning.TracePartitioning;

import javax.swing.*;
import java.awt.event.*;

public class AddIfDirective extends JDialog {
    private JPanel contentPane;
    private JButton buttonOK;
    private JButton buttonCancel;
    private JTextField column;
    private JTextField row;

    public AddIfDirective() {
        setContentPane(contentPane);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onOK();
            }
        });

        buttonCancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCancel();
            }
        });

// call onCancel() when cross is clicked
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onCancel();
            }
        });

// call onCancel() on ESCAPE
        contentPane.registerKeyboardAction(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        onCancel();
                    }
                }, KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    }

    private void onOK() {
// add your code here
        int column = Integer.parseInt(this.column.getText());
        int row = Integer.parseInt(this.row.getText());
        TracePartitioning.add(new PartitionIf(new ScalaProgramPoint(row, column)));
        dispose();
    }

    private void onCancel() {
// add your code here if necessary
        dispose();
    }

    public static void main(String[] args) {
        AddIfDirective dialog = new AddIfDirective();
        dialog.pack();
        dialog.setVisible(true);
        System.exit(0);
    }
}
