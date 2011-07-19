package ch.ethz.inf.pm.sample.userinterfaces.extrapanels;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA. User: dominik Date: 7/9/11 Time: 2:29 PM To change
 * this template use File | Settings | File Templates.
 */
public class Iterations implements Extra {
	private JTextField iterationsField;
	private JLabel errorLabel;
	private JPanel iterationsPanel;

	public int getIterations() {
		int result = -1;
		try {
			result = Integer.parseInt(iterationsField.getText());
		} catch (Exception e) {
		}
		return result;
	}

	public JPanel getPanel() {
		return iterationsPanel;
	}
}
