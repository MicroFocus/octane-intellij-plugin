package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.util.ui.UIUtil;
import org.apache.commons.lang.StringUtils;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import java.awt.*;

class EntityModelRow extends JPanel{

	private static final long serialVersionUID = 1L;

	private JPanel panelDetailsTop;
	private JXLabel lblEntityName;
	private JPanel panelIcon;

	private static final Color transparentColor = new Color(0, 0, 0, 0);
	private Color fontColor = UIUtil.getLabelFontColor(UIUtil.FontColor.NORMAL);

	private JPanel panelDetailsBottom;
	private JXLabel lblEntityRelease;

    private static final EntityIconFactory ENTITY_ICON_FACTORY = new EntityIconFactory(45,45,Color.WHITE);

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
        gbl_rootPanel.rowHeights = new int[]{20, 20, 0};
        gbl_rootPanel.columnWeights = new double[]{0.0, 0.0, 1.0, Double.MIN_VALUE};
        gbl_rootPanel.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
        setLayout(gbl_rootPanel);

        panelIcon = new JPanel();
        FlowLayout flowLayout_1 = (FlowLayout) panelIcon.getLayout();
        flowLayout_1.setVgap(2);
        GridBagConstraints gbc_panelIcon = new GridBagConstraints();
        gbc_panelIcon.gridheight = 2;
        gbc_panelIcon.fill = GridBagConstraints.HORIZONTAL;
        gbc_panelIcon.insets = new Insets(0, 0, 0, 0);
        gbc_panelIcon.gridx = 0;
        gbc_panelIcon.gridy = 0;
        add(panelIcon, gbc_panelIcon);
        panelIcon.setOpaque(true);
        panelIcon.setBackground(transparentColor);
        
        lblEntityName = createLabel();
        lblEntityName.setForeground(fontColor);
        GridBagConstraints gbc_lblEntityName = new GridBagConstraints();
        gbc_lblEntityName.insets = new Insets(5, 0, 0, 0);
        gbc_lblEntityName.anchor = GridBagConstraints.WEST;
        gbc_lblEntityName.gridx = 1;
        gbc_lblEntityName.gridy = 0;
        add(lblEntityName, gbc_lblEntityName);

        panelDetailsTop = new JPanel();
        FlowLayout fl_panelDetailsTop = (FlowLayout) panelDetailsTop.getLayout();
        fl_panelDetailsTop.setVgap(2);
        fl_panelDetailsTop.setHgap(10);
        fl_panelDetailsTop.setAlignment(FlowLayout.TRAILING);
        GridBagConstraints gbc_panelDetailsTop = new GridBagConstraints();
        gbc_panelDetailsTop.insets = new Insets(5, 0, 0, 10);
        gbc_panelDetailsTop.fill = GridBagConstraints.HORIZONTAL;
        gbc_panelDetailsTop.gridx = 2;
        gbc_panelDetailsTop.gridy = 0;
        add(panelDetailsTop, gbc_panelDetailsTop);
        panelDetailsTop.setOpaque(true);
        panelDetailsTop.setBackground(transparentColor);
        
        lblEntityRelease = createLabel();
        lblEntityRelease.setHorizontalAlignment(SwingConstants.LEFT);
        GridBagConstraints gbc_lblEntityId = new GridBagConstraints();
        gbc_lblEntityId.insets = new Insets(0, 0, 5, 0);
        gbc_lblEntityId.anchor = GridBagConstraints.WEST;
        gbc_lblEntityId.gridx = 1;
        gbc_lblEntityId.gridy = 1;
        add(lblEntityRelease, gbc_lblEntityId);
        lblEntityRelease.setForeground(fontColor);
        
        panelDetailsBottom = new JPanel();
        FlowLayout flowLayout = (FlowLayout) panelDetailsBottom.getLayout();
        flowLayout.setVgap(2);
        flowLayout.setHgap(10);
        flowLayout.setAlignment(FlowLayout.TRAILING);
        GridBagConstraints gbc_panelDetailsBottom = new GridBagConstraints();
        gbc_panelDetailsBottom.insets = new Insets(0, 0, 5, 10);
        gbc_panelDetailsBottom.fill = GridBagConstraints.HORIZONTAL;
        gbc_panelDetailsBottom.gridx = 2;
        gbc_panelDetailsBottom.gridy = 1;
        panelDetailsBottom.setOpaque(true);
        panelDetailsBottom.setBackground(transparentColor);
        add(panelDetailsBottom, gbc_panelDetailsBottom);

        setOpaque(true);
    }

	public void setIcon(Entity entityType){
		panelIcon.removeAll();
		panelIcon.add(ENTITY_ICON_FACTORY.getIcon(entityType), BorderLayout.CENTER);
	}
	
	public void setEntityName(String text){
        lblEntityName.setText(text);
	}
	
	public void setEntityDetails(String details, String defaultText){
        if(StringUtils.isEmpty(details)){
            lblEntityRelease.setText(defaultText);
        } else {
            lblEntityRelease.setText(details);
        }
	}
	
    public void addDetailsTop(String text){
    	panelDetailsTop.add(createSeparator());
        panelDetailsTop.add(createLabel(text));
    }

    public void addDetailsBottom(String text){
       	panelDetailsBottom.add(createSeparator());
       	panelDetailsBottom.add(createLabel(text));
    }
    
    private JSeparator createSeparator(){
    	JSeparator separator = new JSeparator(SwingConstants.VERTICAL);
    	separator.setPreferredSize(new Dimension(1, 15));
    	return separator;
    }

    private JXLabel createLabel(){
        JXLabel lbl = new JXLabel();
        lbl.setForeground(fontColor);
        return lbl;
    }

	private JXLabel createLabel(String text){
        JXLabel lbl = new JXLabel(text);
        lbl.setForeground(fontColor);
        return lbl;
    }
	
}