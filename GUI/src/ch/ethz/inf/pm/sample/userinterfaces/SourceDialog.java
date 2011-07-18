package ch.ethz.inf.pm.sample.userinterfaces;

import javax.swing.*;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.Document;
import javax.swing.text.PlainDocument;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

public class SourceDialog extends JDialog {
	private JPanel contentPane;
	private JButton buttonOK;
	private JEditorPane sourcePane;
	private JLabel positionLabel;

	public SourceDialog(final String source) {
		setContentPane(contentPane);
		getRootPane().setDefaultButton(buttonOK);

		sourcePane.setText(source);
		sourcePane.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
		sourcePane.getDocument().putProperty(PlainDocument.tabSizeAttribute, 2);
		sourcePane.setCaretPosition(0);
		sourcePane.addKeyListener(new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				e.consume();
			}

			@Override
			public void keyPressed(KeyEvent e) {
			}

			@Override
			public void keyReleased(KeyEvent e) {
			}
		});
		sourcePane.addCaretListener(new CaretListener() {
			@Override
			public void caretUpdate(CaretEvent e) {
				int line = 1;
				int column = 1;

				for (int i = 0; i < e.getDot() && i < source.length(); i++) {
					switch(source.charAt(i)) {
						case '\n':
							line++;
							column = 1;
							break;
						case '\t':
							column += 8;
							break;
						default:
							column++;
							break;
					}
				}

				positionLabel.setText("Line: " + line + ", Column: " + column);
			}
		});


		buttonOK.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onOK();
			}
		});
	}

	private void onOK() {
		dispose();
	}
}
