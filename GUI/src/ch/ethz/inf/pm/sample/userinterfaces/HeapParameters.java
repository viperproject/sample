package ch.ethz.inf.pm.sample.userinterfaces;

import ch.ethz.inf.pm.sample.abstractdomain.Analysis;
import ch.ethz.inf.pm.sample.property.Property;
import scala.Tuple2;
import scala.collection.Iterator;
import scala.collection.immutable.Set;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.HashMap;

public class HeapParameters extends JDialog {
    private JPanel contentPane;
    private JButton buttonOK;
    private JButton buttonCancel;
    private JPanel mainPanel;
    private Analysis analysis;
    private JComboBox propertyBox;

    public HeapParameters(Analysis a) {
        this.analysis=a;
        initialize();
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
        dispose();
    }

    private void onCancel() {
// add your code here if necessary
        dispose();
    }

    private HashMap<String, JTextField> integerParameters = new HashMap<String, JTextField>();
    private HashMap<String, JCheckBox> booleanParameters = new HashMap<String, JCheckBox>();

    private void initialize() {
        GridBagConstraints c = new GridBagConstraints();
        c.gridwidth = GridBagConstraints.REMAINDER;
        Iterator<Tuple2<String, Boolean>> it = (Iterator<Tuple2<String, Boolean>>) analysis.parameters().iterator();
        while(it.hasNext()) {
            Tuple2<String, Boolean> tuple = (Tuple2<String, Boolean>) it.next();
            if(((Boolean) tuple._2).booleanValue()) {
                JTextField f=new JTextField(5);
                integerParameters.put((String) tuple._1, f);
                mainPanel.add(new JLabel((String) tuple._1));
                mainPanel.add(f,c);
            }
            else  {
                JCheckBox f=new JCheckBox();
                booleanParameters.put((String) tuple._1, f);
                mainPanel.add(new JLabel((String) tuple._1));
                mainPanel.add(f, c);
            }
        }
        Set<Property> properties=(Set<Property>) analysis.getProperties();
        propertyBox=new JComboBox();
        Iterator<Property> it1=(Iterator<Property>) properties.toIterator();
            while(it1.hasNext())
                 propertyBox.addItem(((Property)it1.next()).getLabel());
        mainPanel.add(new JLabel((String) "Property"));
        mainPanel.add(propertyBox, c);
    }


    public static void main(String[] args) {
        HeapParameters dialog = new HeapParameters(null);
        dialog.pack();
        dialog.setVisible(true);
        System.exit(0);
    }
}
