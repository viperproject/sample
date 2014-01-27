package ch.ethz.inf.pm.sample.userinterfaces.extrapanels;

import ch.ethz.inf.pm.sample.tracepartitioning.Directive;
import ch.ethz.inf.pm.sample.tracepartitioning.Merge;
import ch.ethz.inf.pm.sample.tracepartitioning.TracePartitioning;
import scala.collection.JavaConversions;

import javax.swing.*;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

/**
 * Created by IntelliJ IDEA. User: dominik Date: 7/9/11 Time: 2:11 PM To change
 * this template use File | Settings | File Templates.
 */
public class Source implements Extra {
	private JComboBox sourceComboBox;
	private JPanel sourcePanel;
	private DefaultComboBoxModel sourceComboBoxModel;

	private Directive source = null;

	public Source() {
		sourceComboBoxModel = new DefaultComboBoxModel();
		Iterable<Directive<?>> directives = JavaConversions.asJavaIterable(TracePartitioning.getDirectives());
		for (Directive<?> directive : directives) {
			if (!(directive instanceof Merge)) {
				sourceComboBoxModel.addElement(directive);
			}
		}

		if (sourceComboBoxModel.getSize() > 0) {
			source = (Directive) sourceComboBoxModel.getElementAt(0);
		}

		sourceComboBox.setModel(sourceComboBoxModel);
		sourceComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				source = (Directive) e.getItem();
			}
		});
	}

	public Directive getSource() {
		return source;
	}

	public JPanel getPanel() {
		return sourcePanel;
	}
}
