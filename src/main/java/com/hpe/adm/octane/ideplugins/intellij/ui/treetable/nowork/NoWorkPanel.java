/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors (“Open Text”) are as may be set forth
 * in the express warranty statements accompanying such products and services.
 * Nothing herein should be construed as constituting an additional warranty.
 * Open Text shall not be liable for technical or editorial errors or
 * omissions contained herein. The information contained herein is subject
 * to change without notice.
 *
 * Except as specifically indicated otherwise, this document contains
 * confidential information and a valid license is required for possession,
 * use or copying. If this work is provided to the U.S. Government,
 * consistent with FAR 12.211 and 12.212, Commercial Computer Software,
 * Computer Software Documentation, and Technical Data for Commercial Items are
 * licensed to the U.S. Government under vendor's standard commercial license.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork;

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork.snake.SnakeGame;
import com.intellij.util.ui.UIUtil;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

/**
 * Panel shown to the user when his MY WORK tree has no items
 */
public class NoWorkPanel extends JPanel {

	private static final String NO_WORK_TEXT = "You're Awesome! You finished all your work!";
    private static final String NO_WORK_LINK_TEXT = "You may want to talk with your team leader... or have some fun!";

	private JPanel rootPanel;

	public NoWorkPanel() {
		setLayout(new BorderLayout(0, 0));
		rootPanel = new JPanel();
		add(rootPanel);
		rootPanel.setLayout(new BorderLayout(0, 0));
		//default
		showMessage();
	}
	
	public void showMessage(){
		JPanel panelNoWork = new JPanel();
		GridBagLayout gbl_panelNoWork = new GridBagLayout();
		gbl_panelNoWork.columnWidths = new int[]{364, 0};
		gbl_panelNoWork.rowHeights = new int[]{0, 14, 14, 0};
		gbl_panelNoWork.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_panelNoWork.rowWeights = new double[]{1.0, 0.0, 1.0, Double.MIN_VALUE};
		panelNoWork.setLayout(gbl_panelNoWork);
		
		JLabel label = new JLabel("");
		label.setIcon(new ImageIcon(NoWorkPanel.class.getResource(Constants.IMG_NO_WORK_ROCKET)));
		GridBagConstraints gbc_label = new GridBagConstraints();
		gbc_label.anchor = GridBagConstraints.SOUTH;
		gbc_label.insets = new Insets(0, 0, 5, 0);
		gbc_label.gridx = 0;
		gbc_label.gridy = 0;
		panelNoWork.add(label, gbc_label);
		
		JLabel lblCongratulationsForFinishing = new JLabel(NO_WORK_TEXT);
		GridBagConstraints gbc_lblCongratulationsForFinishing = new GridBagConstraints();
		gbc_lblCongratulationsForFinishing.anchor = GridBagConstraints.SOUTH;
		gbc_lblCongratulationsForFinishing.insets = new Insets(0, 0, 5, 0);
		gbc_lblCongratulationsForFinishing.gridx = 0;
		gbc_lblCongratulationsForFinishing.gridy = 1;
		panelNoWork.add(lblCongratulationsForFinishing, gbc_lblCongratulationsForFinishing);
		
		JXHyperlink lblFeelFreeTo = new JXHyperlink(new AbstractAction(NO_WORK_LINK_TEXT) {
			@Override
			public void actionPerformed(ActionEvent e) {
				showGame();
			}
		});
		lblFeelFreeTo.setForeground(UIUtil.getLabelForeground());

		lblFeelFreeTo.setBorder(null);
		GridBagConstraints gbc_lblFeelFreeTo = new GridBagConstraints();
		gbc_lblFeelFreeTo.anchor = GridBagConstraints.NORTH;
		gbc_lblFeelFreeTo.gridx = 0;
		gbc_lblFeelFreeTo.gridy = 2;
		panelNoWork.add(lblFeelFreeTo, gbc_lblFeelFreeTo);
		
		rootPanel.add(panelNoWork, BorderLayout.CENTER);
	}

	/**
	 * Shown when hidden hyperlink is clicked
	 */
	public void showGame(){
		rootPanel.removeAll();
		SnakeGame snakeGame = new SnakeGame();
		rootPanel.add(snakeGame, BorderLayout.CENTER);
		rootPanel.repaint();
		rootPanel.revalidate();
		snakeGame.requestFocus();
	}
}
