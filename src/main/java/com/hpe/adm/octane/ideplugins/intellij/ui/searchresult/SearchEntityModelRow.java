/*
 * © Copyright 2017-2022 Micro Focus or one of its affiliates.
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

package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.util.ui.UIUtil;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import java.awt.*;

public class SearchEntityModelRow extends JPanel {
    @Inject
    private EntityIconFactory iconFactory;

    private static final long serialVersionUID = 1L;
    private JXLabel lblEntityName;
    private JLabel lblEntityIcon;

    private static final Color transparentColor = new Color(0, 0, 0, 0);
    private Color fontColor = UIUtil.getLabelFontColor(UIUtil.FontColor.NORMAL);
    private JXLabel lblEntityRelease;

    public SearchEntityModelRow() {
        super();
    }

    public void initFocusedUI() {
        fontColor = new Color(255, 255, 255);
        initUI();
    }

    public void initUI() {
        GridBagLayout gbl_rootPanel = new GridBagLayout();
        gbl_rootPanel.columnWidths = new int[]{0, 150, 0};
        gbl_rootPanel.rowHeights = new int[]{25, 25, 0};
        gbl_rootPanel.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gbl_rootPanel.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
        setLayout(gbl_rootPanel);

        lblEntityIcon = new JLabel();
        GridBagConstraints gbc_panelIcon = new GridBagConstraints();
        gbc_panelIcon.anchor = GridBagConstraints.CENTER;
        gbc_panelIcon.gridheight = 2;
        gbc_panelIcon.fill = GridBagConstraints.HORIZONTAL;
        gbc_panelIcon.insets = new Insets(5, 0, 5, 5);
        gbc_panelIcon.gridx = 0;
        gbc_panelIcon.gridy = 0;
        add(lblEntityIcon, gbc_panelIcon);
        lblEntityIcon.setOpaque(true);
        lblEntityIcon.setBackground(transparentColor);

        lblEntityName = new JXLabel("");
        lblEntityName.setForeground(fontColor);
        lblEntityName.setHorizontalAlignment(SwingConstants.LEFT);
        lblEntityName.setForeground(fontColor);
        GridBagConstraints gbc_lblEntityName = new GridBagConstraints();
        gbc_lblEntityName.anchor = GridBagConstraints.SOUTH;
        gbc_lblEntityName.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblEntityName.insets = new Insets(5, 0, 5, 0);
        gbc_lblEntityName.gridx = 1;
        gbc_lblEntityName.gridy = 0;
        add(lblEntityName, gbc_lblEntityName);

        lblEntityRelease = new JXLabel("");
        lblEntityRelease.setForeground(fontColor);
        lblEntityRelease.setHorizontalAlignment(SwingConstants.LEFT);
        GridBagConstraints gbc_lblEntityId = new GridBagConstraints();
        gbc_lblEntityId.anchor = GridBagConstraints.NORTH;
        gbc_lblEntityId.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblEntityId.insets = new Insets(0, 0, 5, 0);
        gbc_lblEntityId.gridx = 1;
        gbc_lblEntityId.gridy = 1;
        add(lblEntityRelease, gbc_lblEntityId);
        lblEntityRelease.setForeground(fontColor);

        setOpaque(true);
    }

    public void setIcon(Entity entityType, boolean isActive) {
        ImageIcon icon = new ImageIcon(iconFactory.getIconAsImage(entityType, 40, 16, isActive));
        lblEntityIcon.setIcon(icon);
    }

    public void setEntityName(String id, String name) {
        lblEntityName.setText("<html><body><b>" + id + "</b>&nbsp;" + name + "</body><html>");
    }

    public void setEntityDescription(String description) {
        lblEntityRelease.setText(description);
    }

    public void setEntityHtmlDescription(String description) {
        lblEntityRelease.setText("<html><body>" + description + "</body><html>");
    }

}