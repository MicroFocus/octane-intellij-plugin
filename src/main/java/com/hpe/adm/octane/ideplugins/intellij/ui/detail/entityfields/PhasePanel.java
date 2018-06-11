package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.Collection;

import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;

import org.jdesktop.swingx.JXLabel;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.PhaseComboBox;

public class PhasePanel extends JPanel {

    private JXLabel phaseDetails;
    private JXLabel currentPhaseLabel;
    private JXLabel moveToLabel;

    private PhaseComboBox phaseComboBox;

    private JSeparator separatorPhasePanel;

    public PhasePanel() {

        setToolTipText("");
        setBorder(null);
        GridBagLayout gbl_phasePanel = new GridBagLayout();
        gbl_phasePanel.columnWidths = new int[] { 88, 44, 0, 51, 200 };
        gbl_phasePanel.rowHeights = new int[] { 16, 0 };
        gbl_phasePanel.columnWeights = new double[] { 0.0, 0.0, 0.0, 0.0, 1.0 };
        gbl_phasePanel.rowWeights = new double[] { 1.0, Double.MIN_VALUE };
        setLayout(gbl_phasePanel);

        currentPhaseLabel = new JXLabel();
        currentPhaseLabel.setText("Current phase:");
        currentPhaseLabel.setFont(new Font("Arial", Font.BOLD, 13));
        currentPhaseLabel.setBorder(new EmptyBorder(0, 0, 0, 5));
        GridBagConstraints gbc_currentPhaseLabel = new GridBagConstraints();
        gbc_currentPhaseLabel.anchor = GridBagConstraints.WEST;
        gbc_currentPhaseLabel.insets = new Insets(0, 0, 0, 5);
        gbc_currentPhaseLabel.gridx = 0;
        add(currentPhaseLabel, gbc_currentPhaseLabel);

        phaseDetails = new JXLabel();
        phaseDetails.setText("phase");
        phaseDetails.setFont(new Font("Arial", Font.PLAIN, 13));
        phaseDetails.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_phaseDetails = new GridBagConstraints();
        gbc_phaseDetails.anchor = GridBagConstraints.WEST;
        gbc_phaseDetails.insets = new Insets(0, 0, 0, 5);
        gbc_phaseDetails.gridx = 1;
        add(phaseDetails, gbc_phaseDetails);

        separatorPhasePanel = new JSeparator(SwingConstants.VERTICAL);
        GridBagConstraints gbc_separator3 = new GridBagConstraints();
        gbc_separator3.insets = new Insets(10, 0, 10, 5);
        gbc_separator3.gridx = 2;
        gbc_separator3.fill = GridBagConstraints.VERTICAL;
        gbc_separator3.weighty = 1;
        add(separatorPhasePanel, gbc_separator3);

        moveToLabel = new JXLabel();
        moveToLabel.setText("Move to:");
        moveToLabel.setFont(new Font("Arial", Font.BOLD, 13));
        moveToLabel.setBorder(new EmptyBorder(0, 0, 0, 5));
        GridBagConstraints gbc_moveToLabel = new GridBagConstraints();
        gbc_moveToLabel.anchor = GridBagConstraints.WEST;
        gbc_moveToLabel.insets = new Insets(0, 0, 0, 5);
        gbc_moveToLabel.gridx = 3;
        add(moveToLabel, gbc_moveToLabel);

        phaseComboBox = new PhaseComboBox();
        phaseComboBox.setPreferredSize(new Dimension(150, 30));
        phaseComboBox.setEditable(true);
        GridBagConstraints gbc_phaseComboBox = new GridBagConstraints();
        gbc_phaseComboBox.fill = GridBagConstraints.HORIZONTAL;
        gbc_phaseComboBox.gridx = 4;
        add(phaseComboBox, gbc_phaseComboBox);
    }

    public void setPhaseDetails(String phaseDetails) {
        this.phaseDetails.setText(phaseDetails);
    }

    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
        phaseComboBox.addItems(phasesList);
        if (phasesList.size() == 1) {
            phaseComboBox.setEnabled(false);
        } else {
            phaseComboBox.setEnabled(true);
        }
    }

    public EntityModel getSelectedTransition() {
        EntityModel selectedTransition = (EntityModel) phaseComboBox.getSelectedItem();
        return selectedTransition;
    }

    public void setPhaseInHeader(boolean showPhase) {
        this.setVisible(showPhase);
    }

}
