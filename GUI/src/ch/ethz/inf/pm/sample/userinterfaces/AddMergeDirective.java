package ch.ethz.inf.pm.sample.userinterfaces;

import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaProgramPoint;
import ch.ethz.inf.pm.sample.tracepartitioning.Directive;
import ch.ethz.inf.pm.sample.tracepartitioning.Merge;
import ch.ethz.inf.pm.sample.tracepartitioning.TracePartitioning;
import scala.Tuple2;
import scala.collection.Iterator;
import scala.reflect.ScalaLongSignature;

import javax.swing.*;
import java.awt.event.*;

public class AddMergeDirective extends JDialog {
    private JPanel contentPane;
    private JButton buttonOK;
    private JButton buttonCancel;
    private JTextField column;
    private JTextField row;
    private JComboBox parentDirective;

    public AddMergeDirective() {
        setContentPane(contentPane);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);

        scala.collection.mutable.HashMap map = TracePartitioning.getDirectives();
        Iterator it = map.keySet().iterator();
        while(it.hasNext()) {
            Tuple2 t = (Tuple2) it.next();
            parentDirective.addItem(map.apply(t));
        }

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
        TracePartitioning.add(new Merge(new ScalaProgramPoint(row, column), (Directive) this.parentDirective.getSelectedItem()));
        dispose();
    }

    private void onCancel() {
// add your code here if necessary
        dispose();
    }

    public static void main(String[] args) {
        AddMergeDirective dialog = new AddMergeDirective();
        dialog.pack();
        dialog.setVisible(true);
        System.exit(0);
    }
}
