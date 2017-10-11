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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entity;

import org.jdesktop.swingx.JXPanel;

import java.awt.*;

public class DefectsDetailsPanel extends JXPanel {
    private static final long serialVersionUID = -7172388625845199450L;
    private JXPanel detailsPanelMain;

    public DefectsDetailsPanel() {
        setBorder(null);
        setLayout(new BorderLayout(0, 0));

        detailsPanelMain = new JXPanel();
        detailsPanelMain.setBorder(null);
        add(detailsPanelMain, BorderLayout.CENTER);
        GridBagLayout mainPaneGrid = new GridBagLayout();
        mainPaneGrid.columnWidths = new int[]{500, 500};
        mainPaneGrid.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        mainPaneGrid.columnWeights = new double[]{1.0, 1.0};
        mainPaneGrid.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelMain.setLayout(mainPaneGrid);
        detailsPanelMain.setMinimumSize(new Dimension(0, 0));
    }

    public JXPanel getMainPanel(){
        return detailsPanelMain;
    }
}
