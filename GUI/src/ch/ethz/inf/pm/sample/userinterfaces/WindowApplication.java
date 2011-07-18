package ch.ethz.inf.pm.sample.userinterfaces;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;

import ch.ethz.inf.pm.sample.ScreenOutput;
import ch.ethz.inf.pm.sample.StringCollector;
import ch.ethz.inf.pm.sample.SystemParameters;
import ch.ethz.inf.pm.sample.abstractdomain.*;
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.NonRelationalHeapDomain;
import ch.ethz.inf.pm.sample.property.OutputCollector;
import ch.ethz.inf.pm.sample.tracepartitioning.Directive;
import ch.ethz.inf.pm.sample.tracepartitioning.PartitionedState;
import ch.ethz.inf.pm.sample.tracepartitioning.TracePartitioning;
import scala.Option;
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
	private JList methodList;
	private JButton removeMethodButton;
	private JTextField directiveField;
	private JList directiveList;
	private JButton addDirectiveButton;
	private JButton removeDirectiveButton;
	private JButton displayFileButton;
	private JTextField fileField;
	private JButton addPreconditionButton;
	private File file=null;

	private DefaultListModel methodListModel = new DefaultListModel();
	private DefaultListModel directiveListModel = new DefaultListModel();

    ProgressMonitor progressMonitor;

	public static final JFrame mainFrame = new JFrame("Sample");

    public WindowApplication() {

		/**
		 * Compiler selection
		 */

		compilerComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					try {
						InstalledPlugins.generateTopType(getSelectedCompiler());
					} catch (Exception ex) {
						JOptionPane.showMessageDialog(mainFrame, ex, "Compiler error", JOptionPane.ERROR_MESSAGE);
					}
				}
			}
		});


		/**
		 * Select and display file
		 */

        selectFileButton.addActionListener(new ActionListener() {
			@Override
            public void actionPerformed(ActionEvent e) {
				try {
					JFileChooser fc = new JFileChooser((new File(".")).getCanonicalPath());
					fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
					int returnVal = fc.showOpenDialog(mainFrame);
					if (returnVal == JFileChooser.APPROVE_OPTION) {
						file = fc.getSelectedFile();
						fileField.setText(file.getName());
					}
				} catch (IOException e1) {
				}
            }
        });

		displayFileButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (file != null) {
					try {
						BufferedReader reader = new BufferedReader(new FileReader(file));
						StringBuffer lineBuffer = new StringBuffer();
						String line;
						while ((line = reader.readLine()) != null) {
							lineBuffer.append(line);
							lineBuffer.append("\n");
						}
						SourceDialog dialog = new SourceDialog(lineBuffer.toString());
						dialog.setTitle(file.getName());
						dialog.pack();
						dialog.setLocationRelativeTo(mainFrame);
						dialog.setVisible(true);
					} catch (Exception ex) {
						JOptionPane.showMessageDialog(mainFrame, ex, "Error", JOptionPane.ERROR_MESSAGE);
					}
				}
			}
		});

		/**
		 * Edit methods
		 */

		methodList.setModel(methodListModel);

		methodList.addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				boolean itemSelected = e.getFirstIndex() >= 0;
				removeMethodButton.setEnabled(itemSelected);
				addPreconditionButton.setEnabled(itemSelected);
			}
		});

		methodsTextField.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				String method = methodsTextField.getText().trim();
				if (method.length() > 0) {
					methodListModel.addElement(method);
					methodsTextField.setText("");
				}
			}
		});

        addMethodButton.addActionListener(new ActionListener() {
			@Override
            public void actionPerformed(ActionEvent e) {
				String method = methodsTextField.getText().trim();
				if (method.length() > 0) {
					methodListModel.addElement(method);
					methodsTextField.setText("");
				}
            }
        });

		removeMethodButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				int index = methodList.getSelectedIndex();
				if (index >= 0)	methodListModel.removeElementAt(index);

				removeMethodButton.setEnabled(false);
				addPreconditionButton.setEnabled(false);
				methodList.setSelectedIndex(index - 1);
			}
		});

		addPreconditionButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				Object method = methodList.getSelectedValue();
				String precondition = JOptionPane.showInputDialog(mainFrame, "Precondition for '" + method.toString() + "'", "Precondition", JOptionPane.QUESTION_MESSAGE);
				if (precondition != null && precondition.length() > 0) {
					// TODO
				}
			}
		});

		/**
		 * Edit directives
		 */

		directiveList.setModel(directiveListModel);

		directiveList.addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				boolean itemSelected = e.getFirstIndex() >= 0;
				removeDirectiveButton.setEnabled(itemSelected);
			}
		});

		addDirectiveButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (e.getSource() == addDirectiveButton) {
					String text = directiveField.getText().trim();
					if (text.length() > 0) {
						// Parse
						Option<Directive<?>> directive = Directive.parseUntyped(text);
						if (directive.isDefined()) {
							TracePartitioning.add(directive.get());
							directiveField.setText("");
						} else {
							Option<String> error = Directive.error();
							if (error.isDefined()) {
								JOptionPane.showMessageDialog(mainFrame, error.get(), "Parse Error", JOptionPane.ERROR_MESSAGE);
							}
						}
					} else {
						// Show dialog
						AddDirective dialog = new AddDirective();
						dialog.pack();
						dialog.setLocationRelativeTo(mainFrame);
						dialog.setVisible(true);

						Directive directive = dialog.getDirective();
						if (directive != null) {
							TracePartitioning.add(directive);
							directiveListModel.addElement(directive);
						}
					}
				}
			}
		});

		removeDirectiveButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				int index = directiveList.getSelectedIndex();
				if (index >= 0) {
					Directive directive = (Directive) directiveListModel.get(index);
					TracePartitioning.remove(directive);
					directiveListModel.removeElementAt(index);

					removeDirectiveButton.setEnabled(false);
					directiveList.setSelectedIndex(index - 1);
				}
			}
		});

		/**
		 * Start analysis
		 */

		heapParameters.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				AnalysisParameters a = new AnalysisParameters(getSelectedHeapAnalysis());
				a.pack();
				a.setLocationRelativeTo(mainFrame);
				a.setVisible(true);
			}
		});

		analyzeButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if(file==null) {
					JOptionPane.showMessageDialog(null, "You should chose a file before starting the analysis", "File not chosen", JOptionPane.ERROR_MESSAGE);
					return;
				}
				if(methodListModel.size()==0) {
					JOptionPane.showMessageDialog(null, "You should chose which methods you want to analyze before starting the analysis", "Methods not chosen", JOptionPane.ERROR_MESSAGE);
					return;
				}

				AnalysisParameters a = new AnalysisParameters(getSelectedAnalysis());
				a.pack();
				a.setLocationRelativeTo(mainFrame);
				a.setVisible(true);

				if (a.getResponse() != AnalysisParameters.OK) {
					return;
				}

				//Create and set up the window.
				JFrame frame = new JFrame("ProgressAnalysis");

				//Create and set up the content pane.
				JComponent newContentPane = new ProgressBar(frame);
				newContentPane.setOpaque(true); //content panes must be opaque
				frame.setContentPane(newContentPane);

				//Display the window.
				frame.pack();
				frame.setLocationRelativeTo(mainFrame);
				frame.setVisible(true);
			}
		});

    }

    //This class implements the progress bar displayed while running the analysis
    public class ProgressBar extends JPanel implements PropertyChangeListener {

        private JProgressBar progressBar;
        private Task task;
        private JTextArea taskOutput;

        //This class is used to collect the output of the analysis over the text area
        public class TextAreaProgress implements ScreenOutput{
            public void appendString(String s) {
                 taskOutput.append("\n"+s);
            }
            public String getString() {
                return taskOutput.getText();
            }
        }

        //This class is used to run the analysis while updating the progress bar
        class Task extends SwingWorker<Void, Void> {
            // The core of the analysis
            public Void doInBackground() throws Exception {
                try{
					List<String> methods = List.empty();
					for (Object method : methodListModel.toArray()) {
						methods = methods.$colon$colon(method.toString());
					}

                    ch.ethz.inf.pm.sample.Main.reset();
                    SystemParameters.setProgressOutput(new TextAreaProgress());
                    SystemParameters.setAnalysisOutput(new StringCollector());
                    SystemParameters.heapTimer().reset();
                    SystemParameters.domainTimer().reset();
                    taskOutput.append("\nSetting up the parameters of the analysis");
                    Analysis s = getSelectedAnalysis();
                    s.reset();
                    HeapDomain heap = getSelectedHeapAnalysis();
                    heap.reset();
                    ch.ethz.inf.pm.sample.oorepresentation.Compiler compiler=getSelectedCompiler();
                    if(s==null || heap==null || compiler==null) return null;
                    SystemParameters.resetNativeMethodsSemantics();
                    SystemParameters.addNativeMethodsSemantics(s.getNativeMethodsSemantics());
                    SystemParameters.addNativeMethodsSemantics(heap.getNativeMethodsSemantics());
                    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodsSemantics());
                    SystemParameters.setCompiler(compiler);
                    setProgress(10);
                    taskOutput.append("\nCompiling the files");
                    ch.ethz.inf.pm.sample.Timer tcompiler=new ch.ethz.inf.pm.sample.Timer();
                    tcompiler.start();
                    ch.ethz.inf.pm.sample.Main.compile(file);
                    tcompiler.stop();
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
                    ch.ethz.inf.pm.sample.Timer t=new ch.ethz.inf.pm.sample.Timer();
                    t.start();
                    OutputCollector output = new OutputCollector();
                    if (directiveListModel.size() > 0) {
                        ch.ethz.inf.pm.sample.Main.analyze(methods, new PartitionedState(entryState), output);
					} else {
						ch.ethz.inf.pm.sample.Main.analyze(methods, entryState, output);
					}
                    t.stop();
                    setProgress(100);
                    taskOutput.append("\nAnalysis ended");
                    frame.dispose();

                    SystemParameters.analysisOutput().appendString(output.output() + "\n");
                    SystemParameters.analysisOutput().appendString("Times spent by the compiler:"+tcompiler.totalTime()+" msec");
                    SystemParameters.analysisOutput().appendString("Times spent by the overall analysis:"+t.totalTime()+" msec");
                    SystemParameters.analysisOutput().appendString("Times spent by the heap analysis:"+SystemParameters.heapTimer().totalTime()+" msec");
                    SystemParameters.analysisOutput().appendString("Times spent by the other analysis:"+SystemParameters.domainTimer().totalTime()+" msec");

                    AnalysisResults dialog = new AnalysisResults(SystemParameters.analysisOutput().getString());
                    dialog.pack();
					dialog.setLocationRelativeTo(mainFrame);
                    dialog.setVisible(true);
                    return null;
                }
                catch(Exception e) {
                    JOptionPane.showMessageDialog(null, "Error during the analysis", "Error", JOptionPane.ERROR_MESSAGE);
                    System.out.println(e.toString());
                    e.printStackTrace();
                    progressBar.setVisible(false);
                    throw e;
                }
            }
        }
        JFrame frame;
        public ProgressBar(JFrame frame) {
            super(new BorderLayout());
            this.frame=frame;
			frame.setPreferredSize(new Dimension(500, 400));
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

    private SemanticAnalysis getSelectedAnalysis() {
        String s=analysisComboBox.getSelectedItem().toString();
        for(int i=0; i<InstalledPlugins.analyses.length; i++)
            if(s.equals(InstalledPlugins.analyses[i].getLabel()))
                return InstalledPlugins.analyses[i];
        JOptionPane.showMessageDialog(null, "The analysis you have chosen does not exists", "Unknown analyze", JOptionPane.ERROR_MESSAGE);
        return null;
    }

    private HeapDomain getSelectedHeapAnalysis() {
        String s=heapDomainComboBox.getSelectedItem().toString();
        for(int i=0; i<InstalledPlugins.heapanalyses.length; i++)
            if(s.equals(InstalledPlugins.heapanalyses[i].getLabel()))
                return InstalledPlugins.heapanalyses[i];
        JOptionPane.showMessageDialog(null, "The heap analysis you have chosen does not exists", "Unknown analysis", JOptionPane.ERROR_MESSAGE);
        return null;
    }

    private ch.ethz.inf.pm.sample.oorepresentation.Compiler getSelectedCompiler() {
        String s=compilerComboBox.getSelectedItem().toString();
        for(int i=0; i<InstalledPlugins.compilers.length; i++)
            if(s.equals(InstalledPlugins.compilers[i].getLabel()))
                return InstalledPlugins.compilers[i];
        JOptionPane.showMessageDialog(null, "The compiler you have chosen does not exists", "Unknown compiler", JOptionPane.ERROR_MESSAGE);
        return null;
    }

    public static void main(String[] args) {
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) { e.printStackTrace(); }

        mainFrame.setContentPane(initialize().Sample);
        mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        mainFrame.pack();
		mainFrame.setLocation(10, 10);
        mainFrame.setVisible(true);
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
