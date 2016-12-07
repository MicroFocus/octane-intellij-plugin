package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import java.awt.*;

public class EntityModelRow extends JPanel{

	private static final long serialVersionUID = 1L;
	private JPanel panelDetailsTop;
	private JLabel lblEntityName;
	private JPanel panelIcon;
	
	private static final Color transparentColor = new Color(0, 0, 0, 0);
	private Color fontColor = UIUtil.getLabelFontColor(UIUtil.FontColor.NORMAL);
	private JLabel lblEntityId;
	private JPanel panelDetailsBottom;

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
        gbl_rootPanel.rowHeights = new int[]{0, 0, 0};
        gbl_rootPanel.columnWeights = new double[]{0.0, 0.0, 1.0, Double.MIN_VALUE};
        gbl_rootPanel.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
        setLayout(gbl_rootPanel);

        panelIcon = new JPanel();
        GridBagConstraints gbc_panelIcon = new GridBagConstraints();
        gbc_panelIcon.gridheight = 2;
        gbc_panelIcon.insets = new Insets(0, 0, 0, 5);
        gbc_panelIcon.gridx = 0;
        gbc_panelIcon.gridy = 0;
        add(panelIcon, gbc_panelIcon);
        panelIcon.setOpaque(true);
        panelIcon.setBackground(transparentColor);
        
        lblEntityName = new JLabel("Entity name");
        lblEntityName.setForeground(fontColor);
        GridBagConstraints gbc_lblEntityName = new GridBagConstraints();
        gbc_lblEntityName.anchor = GridBagConstraints.WEST;
        gbc_lblEntityName.insets = new Insets(0, 0, 5, 5);
        gbc_lblEntityName.gridx = 1;
        gbc_lblEntityName.gridy = 0;
        add(lblEntityName, gbc_lblEntityName);

        panelDetailsTop = new JPanel();
        FlowLayout fl_panelDetailsTop = (FlowLayout) panelDetailsTop.getLayout();
        fl_panelDetailsTop.setAlignment(FlowLayout.TRAILING);
        GridBagConstraints gbc_panelDetailsTop = new GridBagConstraints();
        gbc_panelDetailsTop.insets = new Insets(0, 0, 0, 0);
        gbc_panelDetailsTop.fill = GridBagConstraints.HORIZONTAL;
        gbc_panelDetailsTop.gridx = 2;
        gbc_panelDetailsTop.gridy = 0;
        add(panelDetailsTop, gbc_panelDetailsTop);
        panelDetailsTop.setOpaque(true);
        panelDetailsTop.setBackground(transparentColor);

        setOpaque(true);
        
        lblEntityId = new JLabel("Entity Id");
        GridBagConstraints gbc_lblEntityId = new GridBagConstraints();
        gbc_lblEntityId.anchor = GridBagConstraints.WEST;
        gbc_lblEntityId.insets = new Insets(0, 0, 0, 5);
        gbc_lblEntityId.gridx = 1;
        gbc_lblEntityId.gridy = 1;
        add(lblEntityId, gbc_lblEntityId);
        
        panelDetailsBottom = new JPanel();
        FlowLayout flowLayout = (FlowLayout) panelDetailsBottom.getLayout();
        flowLayout.setAlignment(FlowLayout.TRAILING);
        GridBagConstraints gbc_panelDetailsBottom = new GridBagConstraints();
        gbc_panelDetailsBottom.fill = GridBagConstraints.BOTH;
        gbc_panelDetailsBottom.gridx = 2;
        gbc_panelDetailsBottom.gridy = 1;
        panelDetailsBottom.setOpaque(true);
        panelDetailsBottom.setBackground(transparentColor);
        add(panelDetailsBottom, gbc_panelDetailsBottom);
    }

	public void setIcon(JComponent icon){
		panelIcon.removeAll();
		panelIcon.add(icon, BorderLayout.CENTER);
	}
	
	public void setEntityName(String text){
		lblEntityName.setText(text);
	}

	public void addDetailComponentTop(JComponent component){
		panelDetailsTop.add(component);
	}
	
	public void addDetailComponentBottom(JComponent component){
		panelDetailsBottom.add(component);
	}
	
	public void clearDetailsPanel(){
		panelDetailsTop.removeAll();
	}
	
	
}