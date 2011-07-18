package ch.ethz.inf.pm.sample.userinterfaces;

import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint;
import ch.ethz.inf.pm.sample.tracepartitioning.Directive;
import ch.ethz.inf.pm.sample.tracepartitioning.ProgramPointConversions;
import ch.ethz.inf.pm.sample.tracepartitioning.PartitionIf;
import ch.ethz.inf.pm.sample.tracepartitioning.PartitionValue;
import ch.ethz.inf.pm.sample.tracepartitioning.PartitionWhile;
import ch.ethz.inf.pm.sample.tracepartitioning.Merge;
import ch.ethz.inf.pm.sample.tracepartitioning.VariableContext;
import ch.ethz.inf.pm.sample.userinterfaces.extrapanels.Extra;
import ch.ethz.inf.pm.sample.userinterfaces.extrapanels.Context;
import ch.ethz.inf.pm.sample.userinterfaces.extrapanels.Empty;
import ch.ethz.inf.pm.sample.userinterfaces.extrapanels.Iterations;
import ch.ethz.inf.pm.sample.userinterfaces.extrapanels.Source;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class AddDirective extends JDialog {
	private JPanel contentPane;
	private JButton buttonOK;
	private JButton buttonCancel;
	private JTextField lineField;
	private JTextField columnField;

	private JPanel extraPanel;

	private Empty empty = new Empty();
	private Context context = new Context();
	private Iterations iterations = new Iterations();
	private Source source = new Source();

	private JComboBox directiveComboBox;
	private JLabel errorField;
	private DefaultComboBoxModel directiveComboBoxModel;

	private Directive directive = null;

	private static final int IF = 0;
	private static final int VALUE = 1;
	private static final int WHILE = 2;
	private static final int MERGE = 3;

	private String[] directives = new String[]{
			"PartitionIf", "PartitionValue", "PartitionWhile", "Merge"
	};

	private Extra[] extras = new Extra[] {
			empty, context, iterations, source
	};

	private int n = 4;

	public AddDirective() {
		setContentPane(contentPane);
		setModal(true);
		getRootPane().setDefaultButton(buttonOK);

		for (int i = 0; i < n; i++) extraPanel.add(directives[i], extras[i].getPanel());

		directiveComboBoxModel = new DefaultComboBoxModel(directives);

		directiveComboBox.setModel(directiveComboBoxModel);
		directiveComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				CardLayout layout = (CardLayout) extraPanel.getLayout();
				layout.show(extraPanel, (String) e.getItem());
				lineField.requestFocus();
			}
		});

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

		lineField.addFocusListener(new FocusAdapter() {
			@Override
			public void focusGained(FocusEvent focusEvent) {
				lineField.selectAll();
			}
		});

		columnField.addFocusListener(new FocusAdapter() {
			@Override
			public void focusGained(FocusEvent focusEvent) {
				columnField.selectAll();
			}
		});

		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				lineField.requestFocus();
			}
		});
	}

	private void onOK() {
		ProgramPoint pp = getProgramPoint();
		if (pp == null) {
			errorField.setText("Invalid program point");
			return;
		}

		switch (directiveComboBox.getSelectedIndex()) {
			case IF:
				directive = new PartitionIf(pp);
				break;
			case VALUE:
				VariableContext ctx = context.getContext();
				if (ctx == null) {
					errorField.setText("Invalid variable context");
					return;
				}
				directive = new PartitionValue(pp, ctx);
				break;
			case WHILE:
				int i = iterations.getIterations();
				if (i < 0) {
					errorField.setText("Invalid iterations");
					return;
				}
				directive = new PartitionWhile(pp, i);
				break;
			case MERGE:
				Directive src = source.getSource();
				if (src == null) {
					errorField.setText("Invalid source");
					return;
				}
				directive = new Merge(pp, src);
				break;
			default:
				return;
		}
		dispose();
	}

	private void onCancel() {
		directive = null;
		dispose();
	}

	public ProgramPoint getProgramPoint() {
		ProgramPoint result = null;
		try {
			int l = Integer.parseInt(lineField.getText());
			int c = Integer.parseInt(columnField.getText());
			result = ProgramPointConversions.programPoint(l, c);
		} catch (Exception e) {
			// Invalid input
		}
		return result;
	}

	public Directive getDirective() {
		return directive;
	}
}
