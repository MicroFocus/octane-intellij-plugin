package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import javax.swing.JPanel;
import java.awt.GridBagLayout;
import org.jdesktop.swingx.JXLabel;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.FlowLayout;
import java.awt.Component;
import javax.swing.Box;
import java.awt.BorderLayout;
import javax.swing.JLabel;
import org.jdesktop.swingx.JXPanel;
import javax.swing.ImageIcon;
import java.awt.Font;
import javax.swing.border.EmptyBorder;

public class HeaderPanel extends JPanel {
	private JXLabel nameDetails;
	private JXLabel phaseDetails;

	
	public HeaderPanel() {
		setLayout(new BorderLayout(0, 0));
		
		JXPanel nameAndIconPanel = new JXPanel();
		FlowLayout flowLayout = (FlowLayout) nameAndIconPanel.getLayout();
		flowLayout.setHgap(0);
		add(nameAndIconPanel, BorderLayout.WEST);
		
		nameDetails = new JXLabel();
		nameDetails.setIcon(new ImageIcon(HeaderPanel.class.getResource("/images/defectIcon.png")));
		nameDetails.setText("Name");
		nameAndIconPanel.add(nameDetails);
		
		JXPanel phasePanel = new JXPanel();
		FlowLayout flowLayout_1 = (FlowLayout) phasePanel.getLayout();
		flowLayout_1.setHgap(0);
		add(phasePanel, BorderLayout.EAST);
		
		JXLabel currentPhaseLabel = new JXLabel();
		currentPhaseLabel.setBorder(new EmptyBorder(0, 0, 0, 5));
		currentPhaseLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
		currentPhaseLabel.setText("Current phase:");
		phasePanel.add(currentPhaseLabel);
		
		phaseDetails = new JXLabel();
		phaseDetails.setBorder(new EmptyBorder(0, 0, 0, 5));
		phaseDetails.setFont(new Font("Tahoma", Font.ITALIC, 11));
		phaseDetails.setText("phase");
		phasePanel.add(phaseDetails);
		
		JXLabel nextPhaseLink = new JXLabel();
		nextPhaseLink.setFont(new Font("Tahoma", Font.BOLD, 11));
		nextPhaseLink.setText("|  Move to next phase");
		phasePanel.add(nextPhaseLink);

	}

	public void setNameDetails(String nameDetails) {
		this.nameDetails.setText(nameDetails);;
	}

	public void setPhaseDetails(String phaseDetails) {
		this.phaseDetails.setText(phaseDetails);;
	}
	public void setEntityIcon(ImageIcon entityIcon) {
		this.nameDetails.setIcon(entityIcon);
	}
	
}
