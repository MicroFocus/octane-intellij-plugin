package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

import javax.swing.JPanel;
import java.awt.GridBagLayout;
import org.jdesktop.swingx.JXLabel;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import org.jdesktop.swingx.JXTextField;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.Color;

public class OneCommetFatLine extends JPanel {
	private JXLabel userNameLabel;
	private JXLabel postDateLabel;
	private JXTextField textField;

	public OneCommetFatLine() {
		this("", "", "");
	}

	public OneCommetFatLine(String userName, String commetPostDate, String comment) {
		setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[] { 0, 0, 0 };
		gridBagLayout.rowHeights = new int[] { 0, 0, 0 };
		gridBagLayout.columnWeights = new double[] { 0.0, 1.0, Double.MIN_VALUE };
		gridBagLayout.rowWeights = new double[] { 0.0, 0.0, Double.MIN_VALUE };
		setLayout(gridBagLayout);

		userNameLabel = new JXLabel();
		userNameLabel.setText(userName);
		GridBagConstraints gbc_userNameLabel = new GridBagConstraints();
		gbc_userNameLabel.insets = new Insets(0, 0, 5, 5);
		gbc_userNameLabel.gridx = 0;
		gbc_userNameLabel.gridy = 0;
		add(userNameLabel, gbc_userNameLabel);

		postDateLabel = new JXLabel();
		postDateLabel.setBorder(new EmptyBorder(0, 0, 0, 7));
		postDateLabel.setText(commetPostDate);
		GridBagConstraints gbc_postDateLabel = new GridBagConstraints();
		gbc_postDateLabel.insets = new Insets(0, 0, 5, 0);
		gbc_postDateLabel.anchor = GridBagConstraints.EAST;
		gbc_postDateLabel.gridx = 1;
		gbc_postDateLabel.gridy = 0;
		add(postDateLabel, gbc_postDateLabel);

		textField = new JXTextField();
		textField.setText(comment);
		textField.setBorder(null);
		textField.setEditable(false);
		GridBagConstraints gbc_textField = new GridBagConstraints();
		gbc_textField.gridwidth = 2;
		gbc_textField.insets = new Insets(0, 0, 0, 5);
		gbc_textField.fill = GridBagConstraints.HORIZONTAL;
		gbc_textField.gridx = 0;
		gbc_textField.gridy = 1;
		add(textField, gbc_textField);

	}

	public void setUserNameLabel(String userNameLabel) {
		this.userNameLabel.setText(userNameLabel);
	}

	public void setPostDateLabel(String postDateLabel) {
		this.postDateLabel.setText(postDateLabel);
	}

	public void setTextField(String textField) {
		this.textField.setText(textField);
	}

}
