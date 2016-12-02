package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import java.awt.*;

public class EntityModelRow extends JPanel{

	private JPanel panelDetails;
	private JLabel lblEntityName;
	private JPanel panelIcon;
	
	private static final Color transparentColor = new Color(0, 0, 0, 0);
	private Color fontColor = UIUtil.getLabelFontColor(UIUtil.FontColor.NORMAL);

    public EntityModelRow() {
        initUI();
    }

	public EntityModelRow(Color fontColor) {
		this.fontColor = fontColor;
        initUI();
	}

	private void initUI(){
        GridBagLayout gbl_rootPanel = new GridBagLayout();
        gbl_rootPanel.columnWidths = new int[]{0, 0, 0, 0};
        gbl_rootPanel.rowHeights = new int[]{0, 0};
        gbl_rootPanel.columnWeights = new double[]{0.0, 0.0, 1.0, Double.MIN_VALUE};
        gbl_rootPanel.rowWeights = new double[]{0.0, Double.MIN_VALUE};
        setLayout(gbl_rootPanel);

        panelIcon = new JPanel();
        GridBagConstraints gbc_panelIcon = new GridBagConstraints();
        gbc_panelIcon.gridx = 0;
        gbc_panelIcon.gridy = 0;
        add(panelIcon, gbc_panelIcon);
        panelIcon.setOpaque(true);
        panelIcon.setBackground(transparentColor);

        lblEntityName = new JLabel("");
        lblEntityName.setForeground(fontColor);
        GridBagConstraints gbc_lblEntityName = new GridBagConstraints();
        gbc_lblEntityName.gridx = 1;
        gbc_lblEntityName.gridy = 0;
        add(lblEntityName, gbc_lblEntityName);

        panelDetails = new JPanel();
        FlowLayout flowLayout = (FlowLayout) panelDetails.getLayout();
        flowLayout.setAlignment(FlowLayout.TRAILING);
        GridBagConstraints gbc_panelDetails = new GridBagConstraints();
        gbc_panelDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_panelDetails.gridx = 2;
        gbc_panelDetails.gridy = 0;
        add(panelDetails, gbc_panelDetails);
        panelDetails.setOpaque(true);
        panelDetails.setBackground(transparentColor);

        setOpaque(true);
    }

	public void setIcon(JComponent icon){
		panelIcon.removeAll();
		panelIcon.add(icon, BorderLayout.CENTER);
	}
	
	public void setEntityName(String text){
		lblEntityName.setText(text);
	}

	/**
	 * Currently not usable with this layout
	 * @param component
	 */
	public void addDetailComponent(JComponent component){
		panelDetails.add(component);
	}
	
	public void clearDetailsPanel(){
		panelDetails.removeAll();
	}
	
	
}
