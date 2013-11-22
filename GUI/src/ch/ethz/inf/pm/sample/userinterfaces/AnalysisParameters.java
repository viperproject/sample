package ch.ethz.inf.pm.sample.userinterfaces;

import ch.ethz.inf.pm.sample.SystemParameters;
import ch.ethz.inf.pm.sample.abstractdomain.Analysis;
import ch.ethz.inf.pm.sample.abstractdomain.HeapDomain;
import ch.ethz.inf.pm.sample.property.Property;
import scala.Tuple2;
import scala.collection.Iterator;
import scala.collection.immutable.List;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.text.DecimalFormat;
import java.util.HashMap;

public class AnalysisParameters extends JDialog {
    private JPanel contentPane;
    private JButton buttonOK;
    private JButton buttonCancel;
    private JPanel mainPanel;
    private Analysis analysis;
    private JComboBox propertyBox;

	public static final int OK = 1;
	public static final int CANCEL = -1;
	private int response = OK;

    public AnalysisParameters(Analysis a) {
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

        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onCancel();
            }
        });

        contentPane.registerKeyboardAction(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCancel();
            }
        }, KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    }

    private void onOK() {
		response = OK;

        for(String key : this.integerParameters.keySet())
            analysis.setParameter(key, Integer.parseInt(this.integerParameters.get(key).getText()));
        for(String key : this.booleanParameters.keySet())
            analysis.setParameter(key, this.booleanParameters.get(key).isSelected());
        for(String key : this.listParameters.keySet())
            analysis.setParameter(key, this.listParameters.get(key).getSelectedItem());
        List<Property> properties=(List<Property>) analysis.getProperties();
        Iterator<Property> it1=(Iterator<Property>) properties.toIterator();
        while(! (analysis instanceof HeapDomain) && it1.hasNext()) {
           Property p=(Property) it1.next();
           if(p.getLabel().equals(this.propertyBox.getSelectedItem()))
               SystemParameters.setProperty(p);
        }

        dispose();
    }

    private void onCancel() {
		response = CANCEL;
        dispose();
    }

    private HashMap<String, JTextField> integerParameters = new HashMap<String, JTextField>();
    private HashMap<String, JCheckBox> booleanParameters = new HashMap<String, JCheckBox>();
    private HashMap<String, JComboBox> listParameters = new HashMap<String, JComboBox>();

    private void initialize() {
        GridBagConstraints c = new GridBagConstraints();
        c.gridwidth = GridBagConstraints.REMAINDER;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(5, 5, 5, 5);

        Iterator<Tuple2<String, Object>> it = (Iterator<Tuple2<String, Object>>) analysis.parameters().iterator();
        while(it.hasNext()) {
            Tuple2<String, Object> tuple = (Tuple2<String, Object>) it.next();
            JComponent f;
            if(tuple._2 instanceof Integer) {
                f=new JFormattedTextField(new DecimalFormat("####"));
                ((JTextField) f).setColumns(5);
                ((JTextField) f).setText(tuple._2.toString());
                integerParameters.put((String) tuple._1, (JTextField) f);
            }
            else if(tuple._2 instanceof Boolean) {
                f=new JCheckBox();
                ((JCheckBox) f).setSelected(((Boolean) tuple._2).booleanValue());
                booleanParameters.put((String) tuple._1, (JCheckBox) f);
            }
            else if(tuple._2 instanceof scala.collection.immutable.List) {
                scala.collection.immutable.List l = (scala.collection.immutable.List) tuple._2;
                f = new JComboBox();
                for(int i=0; i<l.size(); i++)
                    ((JComboBox) f).addItem(l.apply(i));
                listParameters.put((String) tuple._1, (JComboBox) f);
            }
            else throw new Error("Parameters " + tuple._1 +" of type "+tuple._2+" is not supported");
            JLabel label=new JLabel((String) tuple._1);
            label.setHorizontalAlignment(SwingConstants.LEFT);
            mainPanel.add(label);
            mainPanel.add(f,c);
        }
        if(! (analysis instanceof HeapDomain)) {
            List<Property> properties=(List<Property>) analysis.getProperties();
            propertyBox=new JComboBox();
            Iterator<Property> it1=(Iterator<Property>) properties.toIterator();
                while(it1.hasNext())
                     propertyBox.addItem(((Property)it1.next()).getLabel());
            mainPanel.add(new JLabel((String) "Property"));
            mainPanel.add(propertyBox, c);
        }
    }

	public int getResponse() {
		return response;
	}
}
