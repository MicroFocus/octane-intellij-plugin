/*
 * Copyright 2017 Hewlett-Packard Enterprise Development Company, L.P.
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

package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.ui.JBColor;
import com.intellij.util.ui.UIUtil;
import org.apache.commons.lang.StringUtils;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import java.awt.*;

public class EntityModelRow extends JPanel {
	
    private static final EntityIconFactory entityIconFactory = new EntityIconFactory(40,40,17,Color.WHITE);

    private Color fontColor = UIUtil.getLabelFontColor(UIUtil.FontColor.NORMAL);
	private JPanel panelEntityIcon;
	private JXLabel lblEntityId;
	private JXLabel lblTitle;
	private JXLabel lblSubtitle;
	private JPanel panelDetailsTop;
	private JPanel panelDetailsBottom;
	
	public EntityModelRow() {
		 initUi();
	}
	
	public EntityModelRow(Color fontColor) {
		 this.fontColor = fontColor;
		 initUi();
	}
	
	private void initUi(){
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{40, 150, 0, 0};
		gridBagLayout.rowHeights = new int[]{50, 0};
		gridBagLayout.columnWeights = new double[]{0.0, 1.0, 0.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{0.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);
		
		panelEntityIcon = new JPanel();
		panelEntityIcon.setOpaque(false);
		panelEntityIcon.setMinimumSize(new Dimension(40, 40));
		panelEntityIcon.setPreferredSize(new Dimension(40, 40));
		panelEntityIcon.setMaximumSize(new Dimension(40, 40));
		GridBagConstraints gbc_panelEntityIcon = new GridBagConstraints();
		gbc_panelEntityIcon.fill = GridBagConstraints.BOTH;
		gbc_panelEntityIcon.insets = new Insets(0, 0, 0, 5);
		gbc_panelEntityIcon.gridx = 0;
		gbc_panelEntityIcon.gridy = 0;
		add(panelEntityIcon, gbc_panelEntityIcon);
		panelEntityIcon.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
		
		JPanel panelEntityTitle = new JPanel();
		panelEntityTitle.setOpaque(false);
		GridBagConstraints gbc_panelEntityTitle = new GridBagConstraints();
		gbc_panelEntityTitle.fill = GridBagConstraints.BOTH;
		gbc_panelEntityTitle.insets = new Insets(0, 0, 0, 5);
		gbc_panelEntityTitle.gridx = 1;
		gbc_panelEntityTitle.gridy = 0;
		add(panelEntityTitle, gbc_panelEntityTitle);
		GridBagLayout gbl_panelEntityTitle = new GridBagLayout();
		gbl_panelEntityTitle.columnWidths = new int[]{0, 0, 0};
		gbl_panelEntityTitle.rowHeights = new int[]{0, 0, 0};
		gbl_panelEntityTitle.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
		gbl_panelEntityTitle.rowWeights = new double[]{1.0, 1.0, Double.MIN_VALUE};
		panelEntityTitle.setLayout(gbl_panelEntityTitle);
		
		lblEntityId = createLabel("");
		GridBagConstraints gbc_lblEntityId = new GridBagConstraints();
		gbc_lblEntityId.insets = new Insets(5, 0, 3, 5);
		gbc_lblEntityId.gridx = 0;
		gbc_lblEntityId.gridy = 0;
		panelEntityTitle.add(lblEntityId, gbc_lblEntityId);
		
		lblTitle = createLabel("");
		GridBagConstraints gbc_lblTitle = new GridBagConstraints();
		gbc_lblTitle.insets = new Insets(5, 0, 3, 0);
		gbc_lblTitle.fill = GridBagConstraints.HORIZONTAL;
		gbc_lblTitle.gridx = 1;
		gbc_lblTitle.gridy = 0;
		panelEntityTitle.add(lblTitle, gbc_lblTitle);
		
		lblSubtitle = createLabel("");
		GridBagConstraints gbc_lblSubtitle = new GridBagConstraints();
		gbc_lblSubtitle.insets = new Insets(0, 0, 5, 0);
		gbc_lblSubtitle.fill = GridBagConstraints.HORIZONTAL;
		gbc_lblSubtitle.gridwidth = 2;
		gbc_lblSubtitle.gridx = 0;
		gbc_lblSubtitle.gridy = 1;
		panelEntityTitle.add(lblSubtitle, gbc_lblSubtitle);
		
		JPanel panelEntityDetails = new JPanel();
		panelEntityDetails.setOpaque(false);
		GridBagConstraints gbc_panelEntityDetails = new GridBagConstraints();
		gbc_panelEntityDetails.insets = new Insets(0, 0, 0, 15);
		gbc_panelEntityDetails.fill = GridBagConstraints.BOTH;
		gbc_panelEntityDetails.gridx = 2;
		gbc_panelEntityDetails.gridy = 0;
		add(panelEntityDetails, gbc_panelEntityDetails);
		GridBagLayout gbl_panelEntityDetails = new GridBagLayout();
		gbl_panelEntityDetails.columnWidths = new int[]{0, 0};
		gbl_panelEntityDetails.rowHeights = new int[]{0, 0, 0};
		gbl_panelEntityDetails.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_panelEntityDetails.rowWeights = new double[]{1.0, 1.0, Double.MIN_VALUE};
		panelEntityDetails.setLayout(gbl_panelEntityDetails);
		
		panelDetailsTop = new JPanel();
		panelDetailsTop.setOpaque(false);
		FlowLayout flowLayout_1 = (FlowLayout) panelDetailsTop.getLayout();
		flowLayout_1.setVgap(0);
		flowLayout_1.setAlignment(FlowLayout.TRAILING);
		GridBagConstraints gbc_panelDetailsTop = new GridBagConstraints();
		gbc_panelDetailsTop.insets = new Insets(5, 0, 3, 0);
		gbc_panelDetailsTop.fill = GridBagConstraints.BOTH;
		gbc_panelDetailsTop.gridx = 0;
		gbc_panelDetailsTop.gridy = 0;
		panelEntityDetails.add(panelDetailsTop, gbc_panelDetailsTop);
		
		panelDetailsBottom = new JPanel();
		panelDetailsBottom.setOpaque(false);
		FlowLayout flowLayout = (FlowLayout) panelDetailsBottom.getLayout();
		flowLayout.setVgap(0);
		flowLayout.setAlignment(FlowLayout.TRAILING);
		GridBagConstraints gbc_panelDetailsBottom = new GridBagConstraints();
		gbc_panelDetailsBottom.insets = new Insets(0, 0, 5, 0);
		gbc_panelDetailsBottom.fill = GridBagConstraints.BOTH;
		gbc_panelDetailsBottom.gridx = 0;
		gbc_panelDetailsBottom.gridy = 1;
		panelEntityDetails.add(panelDetailsBottom, gbc_panelDetailsBottom);
	}
	
    public void setIcon(Entity entityType, boolean isActive){
        panelEntityIcon.removeAll();
        panelEntityIcon.add(entityIconFactory.getIconAsComponent(entityType, isActive), BorderLayout.CENTER);
    }

    public void setEntityName(String id, String name){
    	lblEntityId.setText(id);
    	lblTitle.setText("<html><body><span style=\"font-family:'arial unicode ms' , sans-serif\">"+name+"</body></html>");
    }

    public void setEntitySubTitle(String subTitle, String defaultText){
        if(StringUtils.isEmpty(subTitle)){
            lblSubtitle.setText(defaultText);
        } else {
        	lblSubtitle.setText("<html><body><span style=\"font-family:'arial unicode ms' , sans-serif\">"+subTitle+"</body></html>");
        }
    }

    public enum DetailsPosition {
    	TOP, BOTTOM
	}

    public void addDetails(String fieldName, String fieldValue, DetailsPosition position){
    	fieldName = fieldName.trim();
		if(fieldValue == null || StringUtils.isBlank(fieldValue.trim())){
			fieldValue = " - ";
		}

		String lblText = "  " + fieldName + ": " + fieldValue;
        JXLabel lbl = createLabel(lblText);
        lbl.setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, JBColor.border()));

        if(DetailsPosition.TOP.equals(position)){
			panelDetailsTop.add(lbl);
		}
		else if(DetailsPosition.BOTTOM.equals(position)){
			panelDetailsBottom.add(lbl);
		}
    }

    public void addSimpleDetails(String text, DetailsPosition position){
        JXLabel lbl = createLabel("  " + text);
        lbl.setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, JBColor.border()));
        if(DetailsPosition.TOP.equals(position)){
            panelDetailsTop.add(lbl);
        }
        else if(DetailsPosition.BOTTOM.equals(position)){
            panelDetailsBottom.add(lbl);
        }
    }

    private JXLabel createLabel(String text){
        JXLabel lbl = new JXLabel(text);
        lbl.setForeground(fontColor);
        lbl.setFont(new Font("Arial",Font.PLAIN,12));
        return lbl;
    }

}
