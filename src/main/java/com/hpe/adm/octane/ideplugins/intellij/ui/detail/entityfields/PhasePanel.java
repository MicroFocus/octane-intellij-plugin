/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
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

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.PhaseDropDownMenu;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;

public class PhasePanel extends JPanel {

    private JXLabel phaseDetails;
    private JXLabel currentPhaseLabel;
    private JXLabel moveToLabel;

    private PhaseDropDownMenu phaseDropDownMenu;

    private JSeparator separatorPhasePanel;

    public PhasePanel() {

        setToolTipText("");
        setBorder(null);
        GridBagLayout gbl_phasePanel = new GridBagLayout();
        gbl_phasePanel.columnWidths = new int[] { 0, 0, 0, 0, 0 };
        gbl_phasePanel.columnWeights = new double[] { 0.0, 0.0, 0.0, 0.0, 0.0 };
        setLayout(gbl_phasePanel);

        currentPhaseLabel = new JXLabel();
        currentPhaseLabel.setText("Current phase:");
        currentPhaseLabel.setFont(new Font("Arial", Font.BOLD, 14));
        currentPhaseLabel.setBorder(new EmptyBorder(0, 0, 0, 5));
        GridBagConstraints gbc_currentPhaseLabel = new GridBagConstraints();
        gbc_currentPhaseLabel.anchor = GridBagConstraints.WEST;
        gbc_currentPhaseLabel.insets = new Insets(0, 0, 0, 5);
        gbc_currentPhaseLabel.gridx = 0;
        add(currentPhaseLabel, gbc_currentPhaseLabel);

        phaseDetails = new JXLabel();
        phaseDetails.setText("phase");
        phaseDetails.setFont(new Font("Arial", Font.PLAIN, 14));
        phaseDetails.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_phaseDetails = new GridBagConstraints();
        gbc_phaseDetails.anchor = GridBagConstraints.WEST;
        gbc_phaseDetails.insets = new Insets(0, 0, 0, 5);
        gbc_phaseDetails.gridx = 1;
        add(phaseDetails, gbc_phaseDetails);

        separatorPhasePanel = new JSeparator(SwingConstants.VERTICAL);
        GridBagConstraints gbc_separator3 = new GridBagConstraints();
        gbc_separator3.insets = new Insets(0, 0, 0, 5);
        gbc_separator3.gridx = 2;
        gbc_separator3.fill = GridBagConstraints.VERTICAL;
        add(separatorPhasePanel, gbc_separator3);

        phaseDropDownMenu = new PhaseDropDownMenu();
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

    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
        phaseDropDownMenu.addItems((ArrayList) phasesList);
    }

    public FieldModel getSelectedTransition() {
        return phaseDropDownMenu.getSelectedItem();
    }

    public void setPhaseInHeader(boolean showPhase) {
        this.setVisible(showPhase);
    }

}
