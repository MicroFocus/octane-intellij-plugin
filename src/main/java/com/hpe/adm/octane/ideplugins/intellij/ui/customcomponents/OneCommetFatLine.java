/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXTextArea;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;

public class OneCommetFatLine extends JPanel {
	private JXLabel userNameLabel;
	private JXLabel postDateLabel;
	private JXTextArea textField;

	public OneCommetFatLine() {
		this("", "", "");
	}

	public OneCommetFatLine(String userName, String commetPostDate, String comment) {
		setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
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

		textField = new JXTextArea();
		textField.setBackground(JBColor.background());
		textField.setText(comment);
		textField.setLineWrap(true);
		textField.setEditable(false);
		textField.setOpaque(false);
		textField.setEditable(false);
		textField.setBorder(null);
		textField.setFont(new Font("Arial", Font.PLAIN, 11));
		
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
