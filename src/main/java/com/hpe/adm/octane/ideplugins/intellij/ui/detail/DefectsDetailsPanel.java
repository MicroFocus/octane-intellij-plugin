package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.Objects;

public class DefectsDetailsPanel extends JXPanel {
    private static final long serialVersionUID = -7172388625845199450L;
    private JXLabel ownerDetails;
    private JXLabel featureDetails;
    private JXLabel severityDetails;
    private JXLabel sprintDetails;
    private JXLabel storyPointsDetails;
    private JXLabel blockedDetails;
    private JXLabel enviromentDetails;
    private JXLabel releaseDetails;
    private JXLabel defectTypeDetails;
    private JXLabel lastModifiedDetails;
    private JXLabel detectedByDetails;
    private JXLabel teamDetails;
    private JXLabel priorityDetails;
    private JXLabel blockedReasonDetails;
    private JXLabel appModuleDetails;
    private JXLabel detectedInReleaseDetails;
    private JXLabel creationTimeDetails;
    private JXLabel closedOnDetails;


    public DefectsDetailsPanel() {
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
        gbl_detailsPanelLeft.columnWidths = new int[]{0, 0};
        gbl_detailsPanelLeft.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelLeft.columnWeights = new double[]{0.0, 1.0};
        gbl_detailsPanelLeft.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelLeft.setLayout(gbl_detailsPanelLeft);

        JXLabel featureLabel = new JXLabel();
        featureLabel.setText("Feature");
        featureLabel.setFont(new Font("Arial", Font.BOLD, 12));
        featureLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_featureLabel = new GridBagConstraints();
        gbc_featureLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_featureLabel.insets = new Insets(0, 0, 5, 5);
        gbc_featureLabel.gridx = 0;
        gbc_featureLabel.gridy = 0;
        detailsPanelLeft.add(featureLabel, gbc_featureLabel);

        featureDetails = new JXLabel();
        featureDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        featureDetails.setText("        ");
        featureDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_featureDetails = new GridBagConstraints();
        gbc_featureDetails.anchor = GridBagConstraints.SOUTH;
        gbc_featureDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_featureDetails.insets = new Insets(0, 0, 5, 0);
        gbc_featureDetails.gridx = 1;
        gbc_featureDetails.gridy = 0;
        detailsPanelLeft.add(featureDetails, gbc_featureDetails);

        JXLabel severityLabel = new JXLabel();
        severityLabel.setText("Severity");
        severityLabel.setFont(new Font("Arial", Font.BOLD, 12));
        severityLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_severityLabel = new GridBagConstraints();
        gbc_severityLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_severityLabel.insets = new Insets(0, 0, 5, 5);
        gbc_severityLabel.gridx = 0;
        gbc_severityLabel.gridy = 1;
        detailsPanelLeft.add(severityLabel, gbc_severityLabel);

        severityDetails = new JXLabel();
        severityDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        severityDetails.setText("        ");
        severityDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_severityDetails = new GridBagConstraints();
        gbc_severityDetails.anchor = GridBagConstraints.SOUTH;
        gbc_severityDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_severityDetails.insets = new Insets(0, 0, 5, 0);
        gbc_severityDetails.gridx = 1;
        gbc_severityDetails.gridy = 1;
        detailsPanelLeft.add(severityDetails, gbc_severityDetails);

        JXLabel sprintLabel = new JXLabel();
        sprintLabel.setText("Sprint");
        sprintLabel.setFont(new Font("Arial", Font.BOLD, 12));
        sprintLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_sprintLabel = new GridBagConstraints();
        gbc_sprintLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_sprintLabel.insets = new Insets(0, 0, 5, 5);
        gbc_sprintLabel.gridx = 0;
        gbc_sprintLabel.gridy = 2;
        detailsPanelLeft.add(sprintLabel, gbc_sprintLabel);

        sprintDetails = new JXLabel();
        sprintDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        sprintDetails.setText("        ");
        sprintDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_sprintDetails = new GridBagConstraints();
        gbc_sprintDetails.anchor = GridBagConstraints.SOUTH;
        gbc_sprintDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_sprintDetails.insets = new Insets(0, 0, 5, 0);
        gbc_sprintDetails.gridx = 1;
        gbc_sprintDetails.gridy = 2;
        detailsPanelLeft.add(sprintDetails, gbc_sprintDetails);

        JXLabel storyPointsLabel = new JXLabel();
        storyPointsLabel.setText("Story points");
        storyPointsLabel.setFont(new Font("Arial", Font.BOLD, 12));
        storyPointsLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_storyPointsLabel = new GridBagConstraints();
        gbc_storyPointsLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_storyPointsLabel.insets = new Insets(0, 0, 5, 5);
        gbc_storyPointsLabel.gridx = 0;
        gbc_storyPointsLabel.gridy = 3;
        detailsPanelLeft.add(storyPointsLabel, gbc_storyPointsLabel);

        storyPointsDetails = new JXLabel();
        storyPointsDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        storyPointsDetails.setText("        ");
        storyPointsDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_storyPointsDetails = new GridBagConstraints();
        gbc_storyPointsDetails.anchor = GridBagConstraints.SOUTH;
        gbc_storyPointsDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_storyPointsDetails.insets = new Insets(0, 0, 5, 0);
        gbc_storyPointsDetails.gridx = 1;
        gbc_storyPointsDetails.gridy = 3;
        detailsPanelLeft.add(storyPointsDetails, gbc_storyPointsDetails);

        JXLabel blockedLabel = new JXLabel();
        GridBagConstraints gbc_blockedLabel = new GridBagConstraints();
        gbc_blockedLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_blockedLabel.insets = new Insets(0, 0, 5, 5);
        gbc_blockedLabel.gridx = 0;
        gbc_blockedLabel.gridy = 4;
        detailsPanelLeft.add(blockedLabel, gbc_blockedLabel);
        blockedLabel.setText("Blocked");
        blockedLabel.setFont(new Font("Arial", Font.BOLD, 12));
        blockedLabel.setBorder(new EmptyBorder(0, 0, 0, 10));

        blockedDetails = new JXLabel();
        blockedDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        blockedDetails.setText("        ");
        blockedDetails.setBorder(new MatteBorder(0, 0, 1, 0, (JBColor.border())));
        GridBagConstraints gbc_blockedDetails = new GridBagConstraints();
        gbc_blockedDetails.anchor = GridBagConstraints.SOUTH;
        gbc_blockedDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_blockedDetails.insets = new Insets(0, 0, 5, 0);
        gbc_blockedDetails.gridx = 1;
        gbc_blockedDetails.gridy = 4;
        detailsPanelLeft.add(blockedDetails, gbc_blockedDetails);

        JXLabel enviromentLabel = new JXLabel();
        enviromentLabel.setText("Environment");
        enviromentLabel.setFont(new Font("Arial", Font.BOLD, 12));
        enviromentLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_enviromentLabel = new GridBagConstraints();
        gbc_enviromentLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_enviromentLabel.insets = new Insets(0, 0, 5, 5);
        gbc_enviromentLabel.gridx = 0;
        gbc_enviromentLabel.gridy = 5;
        detailsPanelLeft.add(enviromentLabel, gbc_enviromentLabel);

        enviromentDetails = new JXLabel();
        enviromentDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        enviromentDetails.setText("        ");
        enviromentDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_enviromentDetails = new GridBagConstraints();
        gbc_enviromentDetails.anchor = GridBagConstraints.SOUTH;
        gbc_enviromentDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_enviromentDetails.insets = new Insets(0, 0, 5, 0);
        gbc_enviromentDetails.gridx = 1;
        gbc_enviromentDetails.gridy = 5;
        detailsPanelLeft.add(enviromentDetails, gbc_enviromentDetails);

        JXLabel releaseLabel = new JXLabel();
        releaseLabel.setText("Release");
        releaseLabel.setFont(new Font("Arial", Font.BOLD, 12));
        releaseLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_releaseLabel = new GridBagConstraints();
        gbc_releaseLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_releaseLabel.insets = new Insets(0, 0, 5, 5);
        gbc_releaseLabel.gridx = 0;
        gbc_releaseLabel.gridy = 6;
        detailsPanelLeft.add(releaseLabel, gbc_releaseLabel);

        releaseDetails = new JXLabel();
        releaseDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        releaseDetails.setText("        ");
        releaseDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_releaseDetails = new GridBagConstraints();
        gbc_releaseDetails.anchor = GridBagConstraints.SOUTH;
        gbc_releaseDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_releaseDetails.insets = new Insets(0, 0, 5, 0);
        gbc_releaseDetails.gridx = 1;
        gbc_releaseDetails.gridy = 6;
        detailsPanelLeft.add(releaseDetails, gbc_releaseDetails);

        JXLabel defectTypeLabel = new JXLabel();
        defectTypeLabel.setText("Defect type");
        defectTypeLabel.setFont(new Font("Arial", Font.BOLD, 12));
        defectTypeLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_defectTypeLabel = new GridBagConstraints();
        gbc_defectTypeLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_defectTypeLabel.insets = new Insets(0, 0, 5, 5);
        gbc_defectTypeLabel.gridx = 0;
        gbc_defectTypeLabel.gridy = 7;
        detailsPanelLeft.add(defectTypeLabel, gbc_defectTypeLabel);

        defectTypeDetails = new JXLabel();
        defectTypeDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        defectTypeDetails.setText("        ");
        defectTypeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_defectTypeDetails = new GridBagConstraints();
        gbc_defectTypeDetails.anchor = GridBagConstraints.SOUTH;
        gbc_defectTypeDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_defectTypeDetails.insets = new Insets(0, 0, 5, 0);
        gbc_defectTypeDetails.gridx = 1;
        gbc_defectTypeDetails.gridy = 7;
        detailsPanelLeft.add(defectTypeDetails, gbc_defectTypeDetails);

        JXLabel lastModifiedLabel = new JXLabel();
        lastModifiedLabel.setText("Last modified");
        lastModifiedLabel.setFont(new Font("Arial", Font.BOLD, 12));
        lastModifiedLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lastModifiedLabel = new GridBagConstraints();
        gbc_lastModifiedLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_lastModifiedLabel.insets = new Insets(0, 0, 0, 5);
        gbc_lastModifiedLabel.gridx = 0;
        gbc_lastModifiedLabel.gridy = 8;
        detailsPanelLeft.add(lastModifiedLabel, gbc_lastModifiedLabel);

        lastModifiedDetails = new JXLabel();
        lastModifiedDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        lastModifiedDetails.setText("        ");
        lastModifiedDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_lastModifiedDetails = new GridBagConstraints();
        gbc_lastModifiedDetails.anchor = GridBagConstraints.SOUTH;
        gbc_lastModifiedDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_lastModifiedDetails.gridx = 1;
        gbc_lastModifiedDetails.gridy = 8;
        detailsPanelLeft.add(lastModifiedDetails, gbc_lastModifiedDetails);

        JXPanel detailsPanelRight = new JXPanel();
        detailsPanelRight.setBorder(null);
        detailsPanelMain.add(detailsPanelRight, BorderLayout.CENTER);
        GridBagLayout gbl_detailsPanelRight = new GridBagLayout();
        gbl_detailsPanelRight.columnWidths = new int[]{0, 0, 0};
        gbl_detailsPanelRight.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelRight.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gbl_detailsPanelRight.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelRight.setLayout(gbl_detailsPanelRight);

        JXLabel ownerLabel = new JXLabel();
        ownerLabel.setFont(new Font("Arial", Font.BOLD, 12));
        GridBagConstraints gbc_ownerLabel = new GridBagConstraints();
        gbc_ownerLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_ownerLabel.insets = new Insets(0, 0, 5, 5);
        gbc_ownerLabel.gridx = 0;
        gbc_ownerLabel.gridy = 0;
        detailsPanelRight.add(ownerLabel, gbc_ownerLabel);
        ownerLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        ownerLabel.setText("Owner");

        ownerDetails = new JXLabel();
        ownerDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        GridBagConstraints gbc_ownerDetails = new GridBagConstraints();
        gbc_ownerDetails.anchor = GridBagConstraints.SOUTH;
        gbc_ownerDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_ownerDetails.insets = new Insets(0, 0, 5, 0);
        gbc_ownerDetails.gridx = 1;
        gbc_ownerDetails.gridy = 0;
        detailsPanelRight.add(ownerDetails, gbc_ownerDetails);
        ownerDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        ownerDetails.setText("        ");

        JXLabel detectedByLabel = new JXLabel();
        detectedByLabel.setText("Detected by");
        detectedByLabel.setFont(new Font("Arial", Font.BOLD, 12));
        detectedByLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_detectedByLabel = new GridBagConstraints();
        gbc_detectedByLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_detectedByLabel.insets = new Insets(0, 0, 5, 5);
        gbc_detectedByLabel.gridx = 0;
        gbc_detectedByLabel.gridy = 1;
        detailsPanelRight.add(detectedByLabel, gbc_detectedByLabel);

        detectedByDetails = new JXLabel();
        detectedByDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        detectedByDetails.setText("        ");
        detectedByDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_detectedByDetails = new GridBagConstraints();
        gbc_detectedByDetails.anchor = GridBagConstraints.SOUTH;
        gbc_detectedByDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_detectedByDetails.insets = new Insets(0, 0, 5, 0);
        gbc_detectedByDetails.gridx = 1;
        gbc_detectedByDetails.gridy = 1;
        detailsPanelRight.add(detectedByDetails, gbc_detectedByDetails);

        JXLabel teamLabel = new JXLabel();
        teamLabel.setText("Team");
        teamLabel.setFont(new Font("Arial", Font.BOLD, 12));
        teamLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_teamLabel = new GridBagConstraints();
        gbc_teamLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_teamLabel.insets = new Insets(0, 0, 5, 5);
        gbc_teamLabel.gridx = 0;
        gbc_teamLabel.gridy = 2;
        detailsPanelRight.add(teamLabel, gbc_teamLabel);

        teamDetails = new JXLabel();
        teamDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        teamDetails.setText("        ");
        teamDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_teamDetails = new GridBagConstraints();
        gbc_teamDetails.anchor = GridBagConstraints.SOUTH;
        gbc_teamDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_teamDetails.insets = new Insets(0, 0, 5, 0);
        gbc_teamDetails.gridx = 1;
        gbc_teamDetails.gridy = 2;
        detailsPanelRight.add(teamDetails, gbc_teamDetails);

        JXLabel priorityLabel = new JXLabel();
        priorityLabel.setText("Priority");
        priorityLabel.setFont(new Font("Arial", Font.BOLD, 12));
        priorityLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_priorityLabel = new GridBagConstraints();
        gbc_priorityLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_priorityLabel.insets = new Insets(0, 0, 5, 5);
        gbc_priorityLabel.gridx = 0;
        gbc_priorityLabel.gridy = 3;
        detailsPanelRight.add(priorityLabel, gbc_priorityLabel);

        priorityDetails = new JXLabel();
        priorityDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        priorityDetails.setText("        ");
        priorityDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_priorityDetails = new GridBagConstraints();
        gbc_priorityDetails.anchor = GridBagConstraints.SOUTH;
        gbc_priorityDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_priorityDetails.insets = new Insets(0, 0, 5, 0);
        gbc_priorityDetails.gridx = 1;
        gbc_priorityDetails.gridy = 3;
        detailsPanelRight.add(priorityDetails, gbc_priorityDetails);

        JXLabel blockedReasonLabel = new JXLabel();
        blockedReasonLabel.setText("Blocked reason");
        blockedReasonLabel.setFont(new Font("Arial", Font.BOLD, 12));
        blockedReasonLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_blockedReasonLabel = new GridBagConstraints();
        gbc_blockedReasonLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_blockedReasonLabel.insets = new Insets(0, 0, 5, 5);
        gbc_blockedReasonLabel.gridx = 0;
        gbc_blockedReasonLabel.gridy = 4;
        detailsPanelRight.add(blockedReasonLabel, gbc_blockedReasonLabel);

        blockedReasonDetails = new JXLabel();
        blockedReasonDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        blockedReasonDetails.setText("        ");
        blockedReasonDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_blockedReasonDetails = new GridBagConstraints();
        gbc_blockedReasonDetails.anchor = GridBagConstraints.SOUTH;
        gbc_blockedReasonDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_blockedReasonDetails.insets = new Insets(0, 0, 5, 0);
        gbc_blockedReasonDetails.gridx = 1;
        gbc_blockedReasonDetails.gridy = 4;
        detailsPanelRight.add(blockedReasonDetails, gbc_blockedReasonDetails);

        JXLabel appModulesLabel = new JXLabel();
        appModulesLabel.setText("Application module");
        appModulesLabel.setFont(new Font("Arial", Font.BOLD, 12));
        appModulesLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_appModulesLabel = new GridBagConstraints();
        gbc_appModulesLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_appModulesLabel.insets = new Insets(0, 0, 5, 5);
        gbc_appModulesLabel.gridx = 0;
        gbc_appModulesLabel.gridy = 5;
        detailsPanelRight.add(appModulesLabel, gbc_appModulesLabel);

        appModuleDetails = new JXLabel();
        appModuleDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        appModuleDetails.setText("        ");
        appModuleDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_appModuleDetails = new GridBagConstraints();
        gbc_appModuleDetails.anchor = GridBagConstraints.SOUTH;
        gbc_appModuleDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_appModuleDetails.insets = new Insets(0, 0, 5, 0);
        gbc_appModuleDetails.gridx = 1;
        gbc_appModuleDetails.gridy = 5;
        detailsPanelRight.add(appModuleDetails, gbc_appModuleDetails);

        JXLabel detectedInReleaseLabel = new JXLabel();
        detectedInReleaseLabel.setText("Detected in release");
        detectedInReleaseLabel.setFont(new Font("Arial", Font.BOLD, 12));
        detectedInReleaseLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_detectedInReleaseLabel = new GridBagConstraints();
        gbc_detectedInReleaseLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_detectedInReleaseLabel.insets = new Insets(0, 0, 5, 5);
        gbc_detectedInReleaseLabel.gridx = 0;
        gbc_detectedInReleaseLabel.gridy = 6;
        detailsPanelRight.add(detectedInReleaseLabel, gbc_detectedInReleaseLabel);

        detectedInReleaseDetails = new JXLabel();
        detectedInReleaseDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        detectedInReleaseDetails.setText("        ");
        detectedInReleaseDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_detectedInReleaseDetails = new GridBagConstraints();
        gbc_detectedInReleaseDetails.anchor = GridBagConstraints.SOUTH;
        gbc_detectedInReleaseDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_detectedInReleaseDetails.insets = new Insets(0, 0, 5, 0);
        gbc_detectedInReleaseDetails.gridx = 1;
        gbc_detectedInReleaseDetails.gridy = 6;
        detailsPanelRight.add(detectedInReleaseDetails, gbc_detectedInReleaseDetails);

        JXLabel creationTimeLabel = new JXLabel();
        creationTimeLabel.setText("Creation time");
        creationTimeLabel.setFont(new Font("Arial", Font.BOLD, 12));
        creationTimeLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_creationTimeLabel = new GridBagConstraints();
        gbc_creationTimeLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_creationTimeLabel.insets = new Insets(0, 0, 5, 5);
        gbc_creationTimeLabel.gridx = 0;
        gbc_creationTimeLabel.gridy = 7;
        detailsPanelRight.add(creationTimeLabel, gbc_creationTimeLabel);

        creationTimeDetails = new JXLabel();
        creationTimeDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        creationTimeDetails.setText("        ");
        creationTimeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_creationTimeDetails = new GridBagConstraints();
        gbc_creationTimeDetails.anchor = GridBagConstraints.SOUTH;
        gbc_creationTimeDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_creationTimeDetails.insets = new Insets(0, 0, 5, 0);
        gbc_creationTimeDetails.gridx = 1;
        gbc_creationTimeDetails.gridy = 7;
        detailsPanelRight.add(creationTimeDetails, gbc_creationTimeDetails);

        JXLabel closedOnLabel = new JXLabel();
        closedOnLabel.setText("Closed on");
        closedOnLabel.setFont(new Font("Arial", Font.BOLD, 12));
        closedOnLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_closedOnLabel = new GridBagConstraints();
        gbc_closedOnLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_closedOnLabel.insets = new Insets(0, 0, 0, 5);
        gbc_closedOnLabel.gridx = 0;
        gbc_closedOnLabel.gridy = 8;
        detailsPanelRight.add(closedOnLabel, gbc_closedOnLabel);

        closedOnDetails = new JXLabel();
        closedOnDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        closedOnDetails.setText("        ");
        closedOnDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_closedOnDetails = new GridBagConstraints();
        gbc_closedOnDetails.anchor = GridBagConstraints.SOUTH;
        gbc_closedOnDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_closedOnDetails.gridx = 1;
        gbc_closedOnDetails.gridy = 8;
        detailsPanelRight.add(closedOnDetails, gbc_closedOnDetails);

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


    public void setOwnerDetails(String ownerDetails) {
        this.ownerDetails.setText(ownerDetails);
    }


    public void setFeatureDetails(String featureDetails) {
        this.featureDetails.setText(featureDetails);
    }


    public void setSeverityDetails(String severityDetails) {
        this.severityDetails.setText(severityDetails);
    }


    public void setSprintDetails(String sprintDetails) {
        this.sprintDetails.setText(sprintDetails);
    }


    public void setStoryPointsDetails(String storyPointsDetails) {
        this.storyPointsDetails.setText(storyPointsDetails);
    }


    public void setBlockedDetails(String blockedDetails) {
        blockedDetails = (Objects.equals("true", blockedDetails)) ? "yes" : "no";
        this.blockedDetails.setText(blockedDetails);
    }


    public void setEnviromentDetails(String enviromentDetails) {
        this.enviromentDetails.setText(enviromentDetails);
    }


    public void setReleaseDetails(String releaseDetails) {
        this.releaseDetails.setText(releaseDetails);
    }


    public void setDefectTypeDetails(String defectTypeDetails) {
        this.defectTypeDetails.setText(defectTypeDetails);
    }


    public void setLastModifiedDetails(String lastModifiedDetails) {
        this.lastModifiedDetails.setText(lastModifiedDetails);
    }


    public void setDetectedByDetails(String detectedByDetails) {
        this.detectedByDetails.setText(detectedByDetails);
    }


    public void setTeamDetails(String teamDetails) {
        this.teamDetails.setText(teamDetails);
    }


    public void setPriorityDetails(String priorityDetails) {
        this.priorityDetails.setText(priorityDetails);
    }


    public void setBlockedReasonDetails(String blockedReasonDetails) {
        this.blockedReasonDetails.setText(blockedReasonDetails);
    }


    public void setAppModuleDetails(String appModuleDetails) {
        this.appModuleDetails.setText(appModuleDetails);
    }


    public void setDetectedInReleaseDetails(String detectedInReleaseDetails) {
        this.detectedInReleaseDetails.setText(detectedInReleaseDetails);
    }


    public void setCreationTimeDetails(String creationTimeDetails) {
        this.creationTimeDetails.setText(creationTimeDetails);
    }


    public void setClosedOnDetails(String closedOnDetails) {
        this.closedOnDetails.setText(closedOnDetails);
    }


}
