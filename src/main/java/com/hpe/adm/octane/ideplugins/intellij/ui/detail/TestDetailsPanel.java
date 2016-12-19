package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

public class TestDetailsPanel extends JXPanel {
    private static final long serialVersionUID = -7172388625845199450L;
    private JXLabel testTypeDetails;
    private JXLabel testToolTypeDetails;
    private JXLabel estimatedDurationDetails;
    private JXLabel coveredContentDetails;
    private JXLabel applicationModulesDetails;
    private JXLabel ownerDetails;
    private JXLabel designerDetails;
    private JXLabel createdDetails;
    private JXLabel lastModifiedDetails;
    private JXLabel automationStatusLabel;
    private JXLabel automationStatusDetails;


    public TestDetailsPanel() {
        setBorder(null);
        setLayout(new BorderLayout(0, 0));

        JXPanel detailsPanelMain = new JXPanel();
        detailsPanelMain.setBorder(null);
        add(detailsPanelMain, BorderLayout.CENTER);
        detailsPanelMain.setLayout(new BorderLayout(0, 0));
        detailsPanelMain.setMinimumSize(new Dimension(0, 0));

        JXPanel detailsPanelLeft = new JXPanel();
        detailsPanelLeft.setBorder(null);
        detailsPanelMain.add(detailsPanelLeft, BorderLayout.WEST);
        GridBagLayout gbl_detailsPanelLeft = new GridBagLayout();
        gbl_detailsPanelLeft.columnWidths = new int[]{0, 0, 0};
        gbl_detailsPanelLeft.rowHeights = new int[]{0, 0, 0, 0, 0, 0};
        gbl_detailsPanelLeft.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gbl_detailsPanelLeft.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelLeft.setLayout(gbl_detailsPanelLeft);

        JXLabel testTypeLabel = new JXLabel();
        testTypeLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        testTypeLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        testTypeLabel.setText("Test type");
        GridBagConstraints gbc_testTypeLabel = new GridBagConstraints();
        gbc_testTypeLabel.anchor = GridBagConstraints.WEST;
        gbc_testTypeLabel.insets = new Insets(0, 0, 5, 5);
        gbc_testTypeLabel.gridx = 0;
        gbc_testTypeLabel.gridy = 0;
        detailsPanelLeft.add(testTypeLabel, gbc_testTypeLabel);

        testTypeDetails = new JXLabel();
        testTypeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        testTypeDetails.setText("                              ");
        GridBagConstraints gbc_testTypeDetails = new GridBagConstraints();
        gbc_testTypeDetails.anchor = GridBagConstraints.SOUTH;
        gbc_testTypeDetails.insets = new Insets(0, 0, 5, 0);
        gbc_testTypeDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_testTypeDetails.gridx = 1;
        gbc_testTypeDetails.gridy = 0;
        detailsPanelLeft.add(testTypeDetails, gbc_testTypeDetails);

        JXLabel testToolTypeLabel = new JXLabel();
        testToolTypeLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        testToolTypeLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        testToolTypeLabel.setText("Test tool type");
        GridBagConstraints gbc_testToolTypeLabel = new GridBagConstraints();
        gbc_testToolTypeLabel.anchor = GridBagConstraints.WEST;
        gbc_testToolTypeLabel.insets = new Insets(0, 0, 5, 5);
        gbc_testToolTypeLabel.gridx = 0;
        gbc_testToolTypeLabel.gridy = 1;
        detailsPanelLeft.add(testToolTypeLabel, gbc_testToolTypeLabel);

        testToolTypeDetails = new JXLabel();
        testToolTypeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        testToolTypeDetails.setText(" ");
        GridBagConstraints gbc_testToolTypeDetails = new GridBagConstraints();
        gbc_testToolTypeDetails.anchor = GridBagConstraints.SOUTH;
        gbc_testToolTypeDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_testToolTypeDetails.insets = new Insets(0, 0, 5, 0);
        gbc_testToolTypeDetails.gridx = 1;
        gbc_testToolTypeDetails.gridy = 1;
        detailsPanelLeft.add(testToolTypeDetails, gbc_testToolTypeDetails);

        JXLabel estimatedDurationLabel = new JXLabel();
        estimatedDurationLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        estimatedDurationLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        estimatedDurationLabel.setText("Estimated duration");
        GridBagConstraints gbc_estimatedDurationLabel = new GridBagConstraints();
        gbc_estimatedDurationLabel.anchor = GridBagConstraints.WEST;
        gbc_estimatedDurationLabel.insets = new Insets(0, 0, 5, 5);
        gbc_estimatedDurationLabel.gridx = 0;
        gbc_estimatedDurationLabel.gridy = 2;
        detailsPanelLeft.add(estimatedDurationLabel, gbc_estimatedDurationLabel);

        estimatedDurationDetails = new JXLabel();
        estimatedDurationDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_estimatedDurationDetails = new GridBagConstraints();
        gbc_estimatedDurationDetails.anchor = GridBagConstraints.SOUTH;
        gbc_estimatedDurationDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_estimatedDurationDetails.insets = new Insets(0, 0, 5, 0);
        gbc_estimatedDurationDetails.gridx = 1;
        gbc_estimatedDurationDetails.gridy = 2;
        detailsPanelLeft.add(estimatedDurationDetails, gbc_estimatedDurationDetails);

        JXLabel coveredContentLabel = new JXLabel();
        coveredContentLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        coveredContentLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        coveredContentLabel.setText("Covered content");
        GridBagConstraints gbc_coveredContentLabel = new GridBagConstraints();
        gbc_coveredContentLabel.anchor = GridBagConstraints.WEST;
        gbc_coveredContentLabel.insets = new Insets(0, 0, 5, 5);
        gbc_coveredContentLabel.gridx = 0;
        gbc_coveredContentLabel.gridy = 3;
        detailsPanelLeft.add(coveredContentLabel, gbc_coveredContentLabel);

        coveredContentDetails = new JXLabel();
        coveredContentDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_coveredContentDetails = new GridBagConstraints();
        gbc_coveredContentDetails.anchor = GridBagConstraints.SOUTH;
        gbc_coveredContentDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_coveredContentDetails.insets = new Insets(0, 0, 5, 0);
        gbc_coveredContentDetails.gridx = 1;
        gbc_coveredContentDetails.gridy = 3;
        detailsPanelLeft.add(coveredContentDetails, gbc_coveredContentDetails);

        JXLabel applicationModulesLabel = new JXLabel();
        applicationModulesLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        applicationModulesLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        applicationModulesLabel.setText("Application modules");
        GridBagConstraints gbc_applicationModulesLabel = new GridBagConstraints();
        gbc_applicationModulesLabel.insets = new Insets(0, 0, 0, 5);
        gbc_applicationModulesLabel.gridx = 0;
        gbc_applicationModulesLabel.gridy = 4;
        detailsPanelLeft.add(applicationModulesLabel, gbc_applicationModulesLabel);

        applicationModulesDetails = new JXLabel();
        applicationModulesDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_applicationModulesDetails = new GridBagConstraints();
        gbc_applicationModulesDetails.anchor = GridBagConstraints.SOUTH;
        gbc_applicationModulesDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_applicationModulesDetails.gridx = 1;
        gbc_applicationModulesDetails.gridy = 4;
        detailsPanelLeft.add(applicationModulesDetails, gbc_applicationModulesDetails);

        JXPanel detailsPanelRight = new JXPanel();
        detailsPanelRight.setBorder(null);
        detailsPanelMain.add(detailsPanelRight, BorderLayout.CENTER);
        GridBagLayout gbl_detailsPanelRight = new GridBagLayout();
        gbl_detailsPanelRight.columnWidths = new int[]{0, 0, 0};
        gbl_detailsPanelRight.rowHeights = new int[]{0, 0, 0, 0, 0, 0};
        gbl_detailsPanelRight.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gbl_detailsPanelRight.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelRight.setLayout(gbl_detailsPanelRight);

        JXLabel ownerLabel = new JXLabel();
        ownerLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        ownerLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        ownerLabel.setText("Owner");
        GridBagConstraints gbc_ownerLabel = new GridBagConstraints();
        gbc_ownerLabel.anchor = GridBagConstraints.WEST;
        gbc_ownerLabel.insets = new Insets(0, 0, 5, 5);
        gbc_ownerLabel.gridx = 0;
        gbc_ownerLabel.gridy = 0;
        detailsPanelRight.add(ownerLabel, gbc_ownerLabel);

        ownerDetails = new JXLabel();
        ownerDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        ownerDetails.setText(" ");
        GridBagConstraints gbc_ownerDetails = new GridBagConstraints();
        gbc_ownerDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_ownerDetails.anchor = GridBagConstraints.SOUTH;
        gbc_ownerDetails.insets = new Insets(0, 0, 5, 0);
        gbc_ownerDetails.gridx = 1;
        gbc_ownerDetails.gridy = 0;
        detailsPanelRight.add(ownerDetails, gbc_ownerDetails);

        JXLabel designerLabel = new JXLabel();
        designerLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        designerLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        designerLabel.setText("Designer");
        GridBagConstraints gbc_designerLabel = new GridBagConstraints();
        gbc_designerLabel.anchor = GridBagConstraints.WEST;
        gbc_designerLabel.insets = new Insets(0, 0, 5, 5);
        gbc_designerLabel.gridx = 0;
        gbc_designerLabel.gridy = 1;
        detailsPanelRight.add(designerLabel, gbc_designerLabel);

        designerDetails = new JXLabel();
        designerDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_designerDetails = new GridBagConstraints();
        gbc_designerDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_designerDetails.anchor = GridBagConstraints.SOUTH;
        gbc_designerDetails.insets = new Insets(0, 0, 5, 0);
        gbc_designerDetails.gridx = 1;
        gbc_designerDetails.gridy = 1;
        detailsPanelRight.add(designerDetails, gbc_designerDetails);

        JXLabel createdLabel = new JXLabel();
        createdLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        createdLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        createdLabel.setText("Created");
        GridBagConstraints gbc_createdLabel = new GridBagConstraints();
        gbc_createdLabel.anchor = GridBagConstraints.WEST;
        gbc_createdLabel.insets = new Insets(0, 0, 5, 5);
        gbc_createdLabel.gridx = 0;
        gbc_createdLabel.gridy = 2;
        detailsPanelRight.add(createdLabel, gbc_createdLabel);

        createdDetails = new JXLabel();
        createdDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_createdDetails = new GridBagConstraints();
        gbc_createdDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_createdDetails.anchor = GridBagConstraints.SOUTH;
        gbc_createdDetails.insets = new Insets(0, 0, 5, 0);
        gbc_createdDetails.gridx = 1;
        gbc_createdDetails.gridy = 2;
        detailsPanelRight.add(createdDetails, gbc_createdDetails);

        JXLabel lastModifiedLabel = new JXLabel();
        lastModifiedLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        lastModifiedLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        lastModifiedLabel.setText("Last modified");
        GridBagConstraints gbc_lastModifiedLabel = new GridBagConstraints();
        gbc_lastModifiedLabel.anchor = GridBagConstraints.WEST;
        gbc_lastModifiedLabel.insets = new Insets(0, 0, 5, 5);
        gbc_lastModifiedLabel.gridx = 0;
        gbc_lastModifiedLabel.gridy = 3;
        detailsPanelRight.add(lastModifiedLabel, gbc_lastModifiedLabel);

        lastModifiedDetails = new JXLabel();
        lastModifiedDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_lastModifiedDetails = new GridBagConstraints();
        gbc_lastModifiedDetails.insets = new Insets(0, 0, 5, 0);
        gbc_lastModifiedDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_lastModifiedDetails.anchor = GridBagConstraints.SOUTH;
        gbc_lastModifiedDetails.gridx = 1;
        gbc_lastModifiedDetails.gridy = 3;
        detailsPanelRight.add(lastModifiedDetails, gbc_lastModifiedDetails);
        
        automationStatusLabel = new JXLabel();
        automationStatusLabel.setText("Automation status");
        automationStatusLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        automationStatusLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_automationStatusLabel = new GridBagConstraints();
        gbc_automationStatusLabel.insets = new Insets(0, 0, 0, 5);
        gbc_automationStatusLabel.gridx = 0;
        gbc_automationStatusLabel.gridy = 4;
        detailsPanelRight.add(automationStatusLabel, gbc_automationStatusLabel);
        
        automationStatusDetails = new JXLabel();
        automationStatusDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_automationStatusDetails = new GridBagConstraints();
        gbc_automationStatusDetails.anchor = GridBagConstraints.SOUTH;
        gbc_automationStatusDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_automationStatusDetails.gridx = 1;
        gbc_automationStatusDetails.gridy = 4;
        detailsPanelRight.add(automationStatusDetails, gbc_automationStatusDetails);
        
        automationStatusLabel.setVisible(false);
        automationStatusDetails.setVisible(false);
        
        addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent e) {
                int halfWidth = detailsPanelMain.getWidth() / 2;
                int height = detailsPanelMain.getHeight();

                if(halfWidth!=0 && height!=0) {
                    detailsPanelLeft.setPreferredSize(new Dimension((int) halfWidth, detailsPanelMain.getHeight()));
                    detailsPanelRight.setPreferredSize(new Dimension((int) halfWidth, detailsPanelMain.getHeight()));
                    detailsPanelMain.updateUI();
                    detailsPanelMain.repaint();
                }
            }
        });
    }

    public void setTestTypeDetails(String testTypeDetails) {
        this.testTypeDetails.setText(testTypeDetails);
    }

    public void setTestToolTypeDetails(String testToolTypeDetails) {
        this.testToolTypeDetails.setText(testToolTypeDetails);
    }

    public void setEstimatedDurationDetails(String estimatedDurationDetails) {
        this.estimatedDurationDetails.setText(estimatedDurationDetails);
    }

    public void setCoveredContentDetails(String coveredContentDetails) {
        this.coveredContentDetails.setText(coveredContentDetails);
    }

    public void setApplicationModulesDetails(String applicationModulesDetails) {
        this.applicationModulesDetails.setText(applicationModulesDetails);
    }

    public void setOwnerDetails(String ownerDetails) {
        this.ownerDetails.setText(ownerDetails);
    }

    public void setDesignerDetails(String designerDetails) {
        this.designerDetails.setText(designerDetails);
    }

    public void setCreatedDetails(String createdDetails) {
        this.createdDetails.setText(createdDetails);
    }

    public void setLastModifiedDetails(String lastModifiedDetails) {
        this.lastModifiedDetails.setText(lastModifiedDetails);
    }

	public JXLabel getAutomationStatusDetails() {
		return automationStatusDetails;
	}

	public void setAutomationStatusDetails(String automationStatusDetails) {
		this.automationStatusLabel.setVisible(true);
        this.automationStatusDetails.setVisible(true);
		this.automationStatusDetails.setText(automationStatusDetails);
	}
    

}
