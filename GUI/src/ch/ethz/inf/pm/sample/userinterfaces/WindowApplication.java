package ch.ethz.inf.pm.sample.userinterfaces;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;

import ch.ethz.inf.pm.sample.ProgressOutput;
import ch.ethz.inf.pm.sample.SystemParameters;
import ch.ethz.inf.pm.sample.abstractdomain.*;
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.NonRelationalHeapDomain;
import scala.None;
import scala.Some;
import scala.collection.immutable.List;

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
    private JPanel Sample;
    private JButton heapParameters;
    private JButton analyzeButton;
    private JButton selectFileButton;
    private JButton addMethodButton;
    private File file=null;
    private List<String> methods = List.empty();
    ProgressMonitor progressMonitor;

    public WindowApplication() {
        heapParameters.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(e.getSource()==heapParameters) {
                    AnalysisParameters a = new AnalysisParameters(getSelectedHeapAnalysis());
                    a.pack();
                    a.setVisible(true);
                }
            }
        });
        analyzeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(e.getSource()==analyzeButton) {
                    if(file==null) {
                        JOptionPane.showMessageDialog(null, "You should chose a file before starting the analysis", "File not chosen", JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                    if(methods.size()==0) {
                        JOptionPane.showMessageDialog(null, "You should chose which methods you want to analyze before starting the analysis", "Methods not chosen", JOptionPane.ERROR_MESSAGE);
                        return;
                    }

                AnalysisParameters a = new AnalysisParameters(getSelectedAnalysis());
                a.pack();
                a.setVisible(true);
                //Create and set up the window.
                JFrame frame = new JFrame("ProgressAnalysis");
                //frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

                //Create and set up the content pane.
                JComponent newContentPane = new ProgressBar();
                newContentPane.setOpaque(true); //content panes must be opaque
                frame.setContentPane(newContentPane);

                //Display the window.
                frame.pack();
                frame.setVisible(true);
                }
            }
        });
        selectFileButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(e.getSource()==selectFileButton) {
                    JFileChooser fc = new JFileChooser("C:\\Users\\Pietro\\Sample\\AccessPermissionInference\\test\\ChaliceExamples");
                    fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
                    int returnVal = fc.showOpenDialog(Sample);
                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        file = fc.getSelectedFile();
                    }
                }

            }
        });
        addMethodButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(e.getSource()==addMethodButton) {
                     methods=methods.$colon$colon(methodsTextField.getText());
                    methodsTextField.setText("");
                }
            }
        });
    }

    //This class implements the progress bar displayed while running the analysis
    public class ProgressBar extends JPanel implements PropertyChangeListener {

        private JProgressBar progressBar;
        private Task task;
        private JTextArea taskOutput;

        //This class is used to collect the output of the analysis over the text area
        public class TextAreaProgress implements ProgressOutput {
            public void appendString(String s) {
                 taskOutput.append("\n"+s);
            }
        }

        //This class is used to run the analysis while updating the progress bar
        class Task extends SwingWorker<Void, Void> {
            // The core of the analysis
            public Void doInBackground() {
                SystemParameters.setProgressOutput(new TextAreaProgress());
                taskOutput.append("\nSetting up the parameters of the analysis");
                Analysis s = getSelectedAnalysis();
                HeapDomain heap = getSelectedHeapAnalysis();
                SystemParameters.addNativeMethodsSemantics(s.getNativeMethodsSemantics());
                SystemParameters.addNativeMethodsSemantics(heap.getNativeMethodsSemantics());
                SystemParameters.setCompiler(getSelectedCompiler());
                setProgress(10);
                taskOutput.append("\nCompiling the files");
                ch.ethz.inf.pm.sample.Main.compile(file);
                setProgress(40);
                taskOutput.append("\nCreating the initial state of the analysis");
                HeapDomain heapDomain = getSelectedHeapAnalysis();
                if(heapDomain instanceof NonRelationalHeapDomain) {
                    ((NonRelationalHeapDomain) heapDomain).setType(SystemParameters.getType());
                }
                SemanticDomain domain = (SemanticDomain) getSelectedAnalysis().getInitialState();
                HeapAndAnotherDomain entrydomain  = new HeapAndAnotherDomain(domain, heapDomain);
                SymbolicAbstractValue entryvalue =new SymbolicAbstractValue(scala.Option.apply(null), scala.Option.apply(null));
                GenericAbstractState entryState =new GenericAbstractState(entrydomain, entryvalue);
                entryvalue =new SymbolicAbstractValue(new Some(entryState), new Some(SystemParameters.getType()));
                entryState =new GenericAbstractState(entrydomain, entryvalue);
                setProgress(50);
                taskOutput.append("\nRunning the analysis");
                ch.ethz.inf.pm.sample.Main.analyze(methods, entryState);
                setProgress(100);
                taskOutput.append("\nAnalysis ended");
                JOptionPane.showMessageDialog(null, "Analysis successfully ended", "Analysis", JOptionPane.INFORMATION_MESSAGE);
                return null;
            }
        }
        public ProgressBar() {
            super(new BorderLayout());
            //The progress bar
            progressBar = new JProgressBar(0, 100);
            progressBar.setValue(0);
            progressBar.setStringPainted(true);

            //Text output
            taskOutput = new JTextArea(5, 20);
            taskOutput.setMargin(new Insets(5,5,5,5));
            taskOutput.setEditable(false);

            JPanel panel = new JPanel();
            panel.add(progressBar);

            add(panel, BorderLayout.PAGE_START);
            add(new JScrollPane(taskOutput), BorderLayout.CENTER);
            setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));


            //we create new instances as needed.
            task = new Task();
            task.addPropertyChangeListener(this);
            task.execute();

        }

        /**
         * Invoked when task's progress property changes.
         */
        public void propertyChange(PropertyChangeEvent evt) {
            if ("progress" == evt.getPropertyName()) {
                int progress = (Integer) evt.getNewValue();
                progressBar.setValue(progress);
                /*taskOutput.append(String.format(
            "Completed %d%% of task.\n", task.getProgress()));*/
            }
        }

    }

    private Analysis getSelectedAnalysis() {
        String s=analysisComboBox.getSelectedItem().toString();
        for(int i=0; i<InstalledPlugins.analyses.length; i++)
            if(s.equals(InstalledPlugins.analyses[i].getLabel()))
                return InstalledPlugins.analyses[i];
        throw new Error();
    }

    private HeapDomain getSelectedHeapAnalysis() {
        String s=heapDomainComboBox.getSelectedItem().toString();
        for(int i=0; i<InstalledPlugins.heapanalyses.length; i++)
            if(s.equals(InstalledPlugins.heapanalyses[i].getLabel()))
                return InstalledPlugins.heapanalyses[i];
        throw new Error();
    }

    private ch.ethz.inf.pm.sample.oorepresentation.Compiler getSelectedCompiler() {
        String s=compilerComboBox.getSelectedItem().toString();
        for(int i=0; i<InstalledPlugins.compilers.length; i++)
            if(s.equals(InstalledPlugins.compilers[i].getLabel()))
                return InstalledPlugins.compilers[i];
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

        for(int i=0; i<InstalledPlugins.heapanalyses.length; i++)
            myapplication.heapDomainComboBox.addItem(InstalledPlugins.heapanalyses[i].getLabel());

        return myapplication;
    }
}
