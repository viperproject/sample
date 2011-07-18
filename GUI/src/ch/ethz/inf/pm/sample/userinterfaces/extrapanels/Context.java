package ch.ethz.inf.pm.sample.userinterfaces.extrapanels;

import ch.ethz.inf.pm.sample.tracepartitioning.UncheckedVariableContext;
import ch.ethz.inf.pm.sample.tracepartitioning.VariableContext;
import scala.Option;
import scala.Some;
import scala.Tuple2;
import scala.collection.JavaConversions;
import scala.collection.immutable.List;
import scala.runtime.Int;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA. User: dominik Date: 7/9/11 Time: 2:29 PM To change
 * this template use File | Settings | File Templates.
 */
public class Context implements Extra {
	private JPanel contextPanel;
	private JTextField variableField;
	private JTextField lowerField;
	private JTextField upperField;
	private JButton addButton;
	private JButton removeButton;

	private JList boundsList;
	private DefaultListModel boundsListModel;

	public Context() {
		boundsListModel = new DefaultListModel();
		boundsList.setModel(boundsListModel);

		addButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				createBound();
			}
		});

		removeButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				int index = boundsList.getSelectedIndex();
				if (index >= 0) boundsListModel.remove(index);
			}
		});

		lowerField.addFocusListener(new FocusAdapter() {
			@Override
			public void focusGained(FocusEvent focusEvent) {
				lowerField.selectAll();
			}
		});

		lowerField.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				upperField.requestFocus();
			}
		});

		upperField.addFocusListener(new FocusAdapter() {
			@Override
			public void focusGained(FocusEvent focusEvent) {
				upperField.selectAll();
			}
		});

		upperField.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				createBound();
			}
		});

	}

	private void createBound() {
		int lower = Integer.MIN_VALUE;
		int upper = Integer.MAX_VALUE;

		try {
			lower = Integer.parseInt(lowerField.getText());
		} catch (Exception e) {}
		try {
			upper = Integer.parseInt(upperField.getText());
		} catch (Exception e) {}

		boundsListModel.addElement(new Bound(lower, upper));
		lowerField.setText("-inf");
		upperField.setText("inf");
		lowerField.requestFocus();
	}

	public JPanel getPanel() {
		return contextPanel;
	}

	public VariableContext getContext() {
		UncheckedVariableContext result = null;

		int n = boundsListModel.size();
		String variable = variableField.getText().trim();

		if (variable.length() > 0 && n > 0) {
			List bounds = List.empty();
			for (int i = 0; i < n; i++) {
				Bound bound = (Bound) boundsListModel.get(i);
				bounds = bounds.$colon$colon(new Tuple2(bound.lower, bound.upper));
			}

			result = new UncheckedVariableContext(variable, bounds);
		}

		return result;
	}

	private class Bound {
		public int lower = 0;
		public int upper = 0;

		Bound(int l, int u) {
			lower = l;
			upper = u;
		}

		@Override
		public String toString() {
			return "(" + stringRepresentation(lower) + "," + stringRepresentation(upper) + ")";
		}

		private String stringRepresentation(int i) {
			if (i == Integer.MIN_VALUE) return "-inf";
			if (i == Integer.MAX_VALUE) return "inf";
			return Integer.toString(i);
		}
	}
}
