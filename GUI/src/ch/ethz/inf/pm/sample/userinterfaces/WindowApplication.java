package ch.ethz.inf.pm.sample.userinterfaces;

import javax.swing.*;

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
    private JButton parametersButton;
    private JButton parametersButton1;

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
