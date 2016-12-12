package com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork;

import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.intellij.util.ui.UIUtil;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

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
		label.setIcon(new ImageIcon(NoWorkPanel.class.getResource(Constants.IMG_UNIDRAG_SMALL)));
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
		
		JPanel snakePanel = new JPanel();
		rootPanel.add(snakePanel, BorderLayout.CENTER);
		GridBagLayout gbl_snakePanel = new GridBagLayout();
		gbl_snakePanel.columnWidths = new int[]{0, 0};
		gbl_snakePanel.rowHeights = new int[]{0, 0};
		gbl_snakePanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_snakePanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		snakePanel.setLayout(gbl_snakePanel);
        snakePanel.setOpaque(false);

		//80%
		int snakeHeight = (int) rootPanel.getSize().getHeight() * 8/10;
		int snakeWidth = (int) rootPanel.getSize().getWidth() * 8/10;
		SnakeGame snakeBoard = new SnakeGame(new Dimension(snakeWidth, snakeHeight));

		GridBagConstraints gbc_snakeBoard = new GridBagConstraints();
		gbc_snakeBoard.gridx = 0;
		gbc_snakeBoard.gridy = 0;
		snakePanel.add(snakeBoard, gbc_snakeBoard);

		rootPanel.repaint();
		rootPanel.revalidate();
		snakeBoard.requestFocus();
	}
}
