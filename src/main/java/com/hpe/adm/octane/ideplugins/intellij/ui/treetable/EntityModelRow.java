/*******************************************************************************
 * Copyright 2017-2026 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors ("Open Text") are as may be set forth
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

package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.ui.JBColor;
import com.intellij.util.ui.JBUI;
import com.intellij.util.ui.UIUtil;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import java.awt.*;

public class EntityModelRow extends JPanel {

    private EntityIconFactory iconFactory;

    private static final Dimension iconSize = new Dimension(45, 45);
    private static final Insets insets = JBUI.insetsLeft(5);

    private JPanel panelBottom;
    private JPanel panelTop;
    private JLabel lblEntityIcon;
    private JLabel lblEntityId;
    private JLabel lblEntityTitle;
    private JLabel lblEntitySubtitle;
    private Color fontColor;

    public EntityModelRow() {
        this(null, false);
    }

    public EntityModelRow(EntityIconFactory iconFactory) {
        this(iconFactory, false);
    }

    public EntityModelRow(EntityIconFactory iconFactory, boolean isSelected) {
        this.iconFactory = iconFactory;

        setOpaque(false);

        JPanel mainPanel = new JPanel();
        mainPanel.setOpaque(false);

        JPanel detailsPanel = new JPanel();
        detailsPanel.setBorder(JBUI.Borders.emptyRight(10));
        detailsPanel.setOpaque(false);

        setLayout(new EntityModelRowLayoutManager(mainPanel, detailsPanel));

        if (JBColor.isBright()) {
            fontColor = isSelected ? JBColor.background() : JBColor.foreground();
        }

        add(mainPanel, "left");
        add(detailsPanel, "right");

        GridBagLayout gbl_mainPanel = new GridBagLayout();
        gbl_mainPanel.columnWidths = new int[]{0, 0, 0, 0};
        gbl_mainPanel.rowHeights = new int[]{0, 0, 0};
        gbl_mainPanel.columnWeights = new double[]{0.0, 0.0, 1.0, Double.MIN_VALUE};
        gbl_mainPanel.rowWeights = new double[]{1.0, 1.0, Double.MIN_VALUE};
        mainPanel.setLayout(gbl_mainPanel);

        lblEntityIcon = new JLabel();
        lblEntityIcon.setOpaque(false);
        GridBagConstraints gbc_lblIcon = new GridBagConstraints();
        gbc_lblIcon.gridheight = 2;
        gbc_lblIcon.gridx = 0;
        gbc_lblIcon.gridy = 0;
        lblEntityIcon.setMinimumSize(iconSize);
        lblEntityIcon.setMaximumSize(iconSize);
        lblEntityIcon.setPreferredSize(iconSize);
        mainPanel.add(lblEntityIcon, gbc_lblIcon);

        lblEntityId = new JLabel();
        lblEntityId.setForeground(fontColor);
        GridBagConstraints gbc_lblEntityId = new GridBagConstraints();
        gbc_lblEntityId.gridx = 1;
        gbc_lblEntityId.gridy = 0;
        gbc_lblEntityId.insets = insets;
        mainPanel.add(lblEntityId, gbc_lblEntityId);

        lblEntityTitle = new JLabel();
        lblEntityTitle.setForeground(fontColor);
        GridBagConstraints gbc_lblEntityTitle = new GridBagConstraints();
        gbc_lblEntityTitle.anchor = GridBagConstraints.WEST;
        gbc_lblEntityTitle.gridx = 2;
        gbc_lblEntityTitle.gridy = 0;
        gbc_lblEntityTitle.insets = insets;
        mainPanel.add(lblEntityTitle, gbc_lblEntityTitle);

        lblEntitySubtitle = new JLabel();
        lblEntitySubtitle.setHorizontalAlignment(SwingConstants.LEFT);
        lblEntitySubtitle.setForeground(fontColor);
        GridBagConstraints gbc_lblEntitySubtitle = new GridBagConstraints();
        gbc_lblEntitySubtitle.anchor = GridBagConstraints.WEST;
        gbc_lblEntitySubtitle.gridx = 1;
        gbc_lblEntitySubtitle.gridy = 1;
        gbc_lblEntitySubtitle.gridwidth = 2;
        gbc_lblEntitySubtitle.insets = insets;
        mainPanel.add(lblEntitySubtitle, gbc_lblEntitySubtitle);

        GridBagLayout gbl_detailsPanel = new GridBagLayout();
        gbl_detailsPanel.columnWidths = new int[]{0, 0};
        gbl_detailsPanel.rowHeights = new int[]{0, 0, 0};
        gbl_detailsPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_detailsPanel.rowWeights = new double[]{1.0, 1.0, Double.MIN_VALUE};
        detailsPanel.setLayout(gbl_detailsPanel);

        FlowLayout flowLayoutTop = new FlowLayout(FlowLayout.TRAILING);
        flowLayoutTop.setVgap(0);
        flowLayoutTop.setHgap(10);
        panelTop = new JPanel(flowLayoutTop);
        panelTop.setOpaque(false);
        GridBagConstraints gbc_panelTop = new GridBagConstraints();
        gbc_panelTop.fill = GridBagConstraints.HORIZONTAL;
        gbc_panelTop.gridx = 0;
        gbc_panelTop.gridy = 0;
        detailsPanel.add(panelTop, gbc_panelTop);

        FlowLayout flowLayoutBottom = new FlowLayout(FlowLayout.TRAILING);
        flowLayoutBottom.setVgap(0);
        flowLayoutBottom.setHgap(10);
        panelBottom = new JPanel(flowLayoutBottom);
        panelBottom.setOpaque(false);
        GridBagConstraints gbc_panelBottom = new GridBagConstraints();
        gbc_panelBottom.fill = GridBagConstraints.HORIZONTAL;
        gbc_panelBottom.gridx = 0;
        gbc_panelBottom.gridy = 1;
        detailsPanel.add(panelBottom, gbc_panelBottom);
    }

    public void setIcon(Entity entityType, boolean isActive) {
        if (iconFactory != null) {
            iconFactory.getIconAsImageAsync(entityType, 40, 17, isActive, icon -> lblEntityIcon.setIcon(new ImageIcon(icon)));
        }
    }

    public void setEntityName(String id, String name) {
        lblEntityId.setText(id);
        lblEntityTitle.setText(name);
    }

    public void setEntitySubTitle(String subTitle, String defaultText) {
        if (subTitle == null || subTitle.trim().isEmpty()) {
            lblEntitySubtitle.setText(defaultText);
        } else {
            lblEntitySubtitle.setText(subTitle);
        }
    }

    public void addDetails(String fieldName, String fieldValue, DetailsPosition position) {
        fieldName = fieldName.trim();
        if (fieldValue == null || fieldValue.trim().isEmpty()) {
            fieldValue = " - ";
        }

        String lblText = "  " + fieldName + ": " + fieldValue;
        JXLabel lbl = createLabel(lblText);
        lbl.setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, JBColor.border()));

        if (DetailsPosition.TOP.equals(position)) {
            panelTop.add(lbl);
        } else if (DetailsPosition.BOTTOM.equals(position)) {
            panelBottom.add(lbl);
        }
    }

    public void addSimpleDetails(String text, DetailsPosition position) {
        JXLabel lbl = createLabel("  " + text);
        if (DetailsPosition.TOP.equals(position)) {
            panelTop.add(lbl);
        } else if (DetailsPosition.BOTTOM.equals(position)) {
            panelBottom.add(lbl);
        }
    }

    private JXLabel createLabel(String text) {
        JXLabel label = new JXLabel(text);
        label.setForeground(fontColor);
        return label;
    }

}