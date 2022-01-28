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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.PhaseDropDownMenu;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.util.ui.JBUI;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import java.awt.*;

public class PhasePanel extends JPanel {

    private JXLabel phaseDetails;

    private PhaseDropDownMenu phaseDropDownMenu;

    @Inject
    public PhasePanel(PhaseDropDownMenu phaseDropDownMenu) {
        this.phaseDropDownMenu = phaseDropDownMenu;
        setToolTipText("");
        setBorder(null);
        GridBagLayout gbl_phasePanel = new GridBagLayout();
        gbl_phasePanel.columnWidths = new int[]{0, 0, 0, 0, 0};
        gbl_phasePanel.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0};
        setLayout(gbl_phasePanel);

        JXLabel currentPhaseLabel = new JXLabel();
        currentPhaseLabel.setText("Current phase:");
        currentPhaseLabel.setFont(new Font(currentPhaseLabel.getFont().getName(), Font.BOLD, 14));
        currentPhaseLabel.setBorder(JBUI.Borders.emptyRight(5));
        GridBagConstraints gbc_currentPhaseLabel = new GridBagConstraints();
        gbc_currentPhaseLabel.anchor = GridBagConstraints.WEST;
        gbc_currentPhaseLabel.insets = JBUI.insets(5, 0, 5, 5);
        gbc_currentPhaseLabel.gridx = 0;
        add(currentPhaseLabel, gbc_currentPhaseLabel);

        phaseDetails = new JXLabel();
        phaseDetails.setText("phase");
        phaseDetails.setFont(new Font(phaseDetails.getFont().getName(), Font.PLAIN, 14));
        phaseDetails.setBorder(JBUI.Borders.emptyRight(10));
        GridBagConstraints gbc_phaseDetails = new GridBagConstraints();
        gbc_phaseDetails.anchor = GridBagConstraints.WEST;
        gbc_phaseDetails.insets = JBUI.insets(5, 0, 5, 5);
        gbc_phaseDetails.gridx = 1;
        add(phaseDetails, gbc_phaseDetails);

        JSeparator separatorPhasePanel = new JSeparator(SwingConstants.VERTICAL);
        GridBagConstraints gbc_separator3 = new GridBagConstraints();
        gbc_separator3.gridx = 2;
        gbc_separator3.fill = GridBagConstraints.VERTICAL;
        add(separatorPhasePanel, gbc_separator3);

        GridBagConstraints gbc_phaseComboBox = new GridBagConstraints();
        gbc_phaseComboBox.fill = GridBagConstraints.HORIZONTAL;
        gbc_phaseComboBox.anchor = GridBagConstraints.WEST;
        gbc_phaseComboBox.gridx = 3;
        add(phaseDropDownMenu, gbc_phaseComboBox);
    }

    public void setPhaseDetails(FieldModel phaseDetails) {
        this.phaseDetails.setText(Util.getUiDataFromModel(phaseDetails));
        this.phaseDropDownMenu.setPhaseDetails(phaseDetails);
    }

    public void setEntityModelWrapper(EntityModelWrapper entityModelWrapper) {
        phaseDropDownMenu.setEntityModelWrapper(entityModelWrapper);
    }

    public void setPhaseInHeader(boolean showPhase) {
        this.setVisible(showPhase);
    }

}
