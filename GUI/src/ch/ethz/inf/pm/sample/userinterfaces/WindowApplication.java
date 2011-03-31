package ch.ethz.inf.pm.sample.userinterfaces;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import ch.ethz.inf.pm.sample.abstractdomain.Analysis;

/**
 * Created by IntelliJ IDEA.
 * User: Pietro
 * Date: 29.03.11
 * Time: 13:36
 * To change this template use File | Settings | File Templates.
 */
public class WindowApplication {
    private JComboBox compilerComboBox;
    private JComboBox analysisComboBox;
    private JComboBox heapDomainComboBox;
    private JTextField methodsTextField;
    private JTextField filesTextField;
    private JPanel Sample;
    private JButton analysisParameters;
    private JButton heapParameters;

    public WindowApplication() {
        analysisParameters.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                int i=0;
                if(e.getSource()==analysisParameters) {
                    AnalysisParameters a = new AnalysisParameters(getSelectedAnalysis());
                    a.pack();
                    a.setVisible(true);
                }
            }
        });
    }

    private Analysis getSelectedAnalysis() {
        String s=analysisComboBox.getSelectedItem().toString();
        for(int i=0; i<InstalledPlugins.analyses.length; i++)
            if(s.equals(InstalledPlugins.analyses[i].getLabel()))
                return InstalledPlugins.analyses[i];
        throw new Error();
    }

    public static void main(String[] args) {
        JFrame frame = new JFrame("WindowApplication");
        frame.setContentPane(initialize().Sample);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }

    private static WindowApplication initialize() {
        WindowApplication myapplication=new WindowApplication();
        for(int i=0; i<InstalledPlugins.compilers.length; i++)
            myapplication.compilerComboBox.addItem(InstalledPlugins.compilers[i].getLabel());

        for(int i=0; i<InstalledPlugins.analyses.length; i++)
            myapplication.analysisComboBox.addItem(InstalledPlugins.analyses[i].getLabel());
        return myapplication;
    }
}
