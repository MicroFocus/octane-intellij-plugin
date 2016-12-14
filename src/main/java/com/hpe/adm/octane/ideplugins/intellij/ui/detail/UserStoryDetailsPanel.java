package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

public class UserStoryDetailsPanel extends JXPanel {
    private static final long serialVersionUID = -7172388625845199450L;
    private JXLabel ownerDetails;
    private JXLabel featureDetails;
    private JXLabel sprintDetails;
    private JXLabel storyPointsDetails;
    private JXLabel blockedDetails;
    private JXLabel enviromentDetails;
    private JXLabel releaseDetails;
    private JXLabel lastModifiedDetails;
    private JXLabel teamDetails;
    private JXLabel blockedReasonDetails;
    private JXLabel appModuleDetails;
    private JXLabel creationTimeDetails;
    private JXLabel lastRunsDetails;
    private JXLabel itemOriginDetails;


    public UserStoryDetailsPanel() {
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
        gbl_detailsPanelLeft.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelLeft.columnWeights = new double[]{0.0, 1.0};
        gbl_detailsPanelLeft.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelLeft.setLayout(gbl_detailsPanelLeft);

        JXLabel ownerLabel = new JXLabel();
        GridBagConstraints gbc_ownerLabel = new GridBagConstraints();
        gbc_ownerLabel.anchor = GridBagConstraints.WEST;
        gbc_ownerLabel.insets = new Insets(0, 0, 5, 5);
        gbc_ownerLabel.gridx = 0;
        gbc_ownerLabel.gridy = 0;
        detailsPanelLeft.add(ownerLabel, gbc_ownerLabel);
        ownerLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        ownerLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        ownerLabel.setText("Owner");

        ownerDetails = new JXLabel();
        GridBagConstraints gbc_ownerDetails = new GridBagConstraints();
        gbc_ownerDetails.anchor = GridBagConstraints.SOUTH;
        gbc_ownerDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_ownerDetails.insets = new Insets(0, 0, 5, 0);
        gbc_ownerDetails.gridx = 1;
        gbc_ownerDetails.gridy = 0;
        detailsPanelLeft.add(ownerDetails, gbc_ownerDetails);
        ownerDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        ownerDetails.setText(" ");

        JXLabel featureLabel = new JXLabel();
        featureLabel.setText("Feature");
        featureLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        featureLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_featureLabel = new GridBagConstraints();
        gbc_featureLabel.anchor = GridBagConstraints.WEST;
        gbc_featureLabel.insets = new Insets(0, 0, 5, 5);
        gbc_featureLabel.gridx = 0;
        gbc_featureLabel.gridy = 1;
        detailsPanelLeft.add(featureLabel, gbc_featureLabel);

        featureDetails = new JXLabel();
        featureDetails.setText("                                          ");
        featureDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_featureDetails = new GridBagConstraints();
        gbc_featureDetails.anchor = GridBagConstraints.SOUTH;
        gbc_featureDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_featureDetails.insets = new Insets(0, 0, 5, 0);
        gbc_featureDetails.gridx = 1;
        gbc_featureDetails.gridy = 1;
        detailsPanelLeft.add(featureDetails, gbc_featureDetails);

        JXLabel sprintLabel = new JXLabel();
        sprintLabel.setText("Sprint");
        sprintLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        sprintLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_sprintLabel = new GridBagConstraints();
        gbc_sprintLabel.anchor = GridBagConstraints.WEST;
        gbc_sprintLabel.insets = new Insets(0, 0, 5, 5);
        gbc_sprintLabel.gridx = 0;
        gbc_sprintLabel.gridy = 2;
        detailsPanelLeft.add(sprintLabel, gbc_sprintLabel);

        sprintDetails = new JXLabel();
        sprintDetails.setText(" ");
        sprintDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_sprintDetails = new GridBagConstraints();
        gbc_sprintDetails.anchor = GridBagConstraints.SOUTH;
        gbc_sprintDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_sprintDetails.insets = new Insets(0, 0, 5, 0);
        gbc_sprintDetails.gridx = 1;
        gbc_sprintDetails.gridy = 2;
        detailsPanelLeft.add(sprintDetails, gbc_sprintDetails);

        JXLabel storyPointsLabel = new JXLabel();
        storyPointsLabel.setText("Story Points");
        storyPointsLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        storyPointsLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_storyPointsLabel = new GridBagConstraints();
        gbc_storyPointsLabel.anchor = GridBagConstraints.WEST;
        gbc_storyPointsLabel.insets = new Insets(0, 0, 5, 5);
        gbc_storyPointsLabel.gridx = 0;
        gbc_storyPointsLabel.gridy = 3;
        detailsPanelLeft.add(storyPointsLabel, gbc_storyPointsLabel);

        storyPointsDetails = new JXLabel();
        storyPointsDetails.setText(" ");
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
        gbc_blockedLabel.anchor = GridBagConstraints.WEST;
        gbc_blockedLabel.insets = new Insets(0, 0, 5, 5);
        gbc_blockedLabel.gridx = 0;
        gbc_blockedLabel.gridy = 4;
        detailsPanelLeft.add(blockedLabel, gbc_blockedLabel);
        blockedLabel.setText("Blocked");
        blockedLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        blockedLabel.setBorder(new EmptyBorder(0, 0, 0, 10));

        blockedDetails = new JXLabel();
        blockedDetails.setText(" ");
        blockedDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_blockedDetails = new GridBagConstraints();
        gbc_blockedDetails.anchor = GridBagConstraints.SOUTH;
        gbc_blockedDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_blockedDetails.insets = new Insets(0, 0, 5, 0);
        gbc_blockedDetails.gridx = 1;
        gbc_blockedDetails.gridy = 4;
        detailsPanelLeft.add(blockedDetails, gbc_blockedDetails);

        JXLabel lastRunsRuns = new JXLabel();
        lastRunsRuns.setBorder(new EmptyBorder(0, 0, 0, 10));
        lastRunsRuns.setFont(new Font("Tahoma", Font.BOLD, 11));
        lastRunsRuns.setText("Last runs");
        GridBagConstraints gbc_lastRunsRuns = new GridBagConstraints();
        gbc_lastRunsRuns.anchor = GridBagConstraints.WEST;
        gbc_lastRunsRuns.insets = new Insets(0, 0, 5, 5);
        gbc_lastRunsRuns.gridx = 0;
        gbc_lastRunsRuns.gridy = 5;
        detailsPanelLeft.add(lastRunsRuns, gbc_lastRunsRuns);

        lastRunsDetails = new JXLabel();
        lastRunsDetails.setText(" ");
        lastRunsDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_lastRunsDetails = new GridBagConstraints();
        gbc_lastRunsDetails.anchor = GridBagConstraints.SOUTH;
        gbc_lastRunsDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_lastRunsDetails.insets = new Insets(0, 0, 5, 0);
        gbc_lastRunsDetails.gridx = 1;
        gbc_lastRunsDetails.gridy = 5;
        detailsPanelLeft.add(lastRunsDetails, gbc_lastRunsDetails);

        JXLabel lastModifiedLabel = new JXLabel();
        lastModifiedLabel.setText("Last modified");
        lastModifiedLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        lastModifiedLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lastModifiedLabel = new GridBagConstraints();
        gbc_lastModifiedLabel.anchor = GridBagConstraints.WEST;
        gbc_lastModifiedLabel.insets = new Insets(0, 0, 0, 5);
        gbc_lastModifiedLabel.gridx = 0;
        gbc_lastModifiedLabel.gridy = 6;
        detailsPanelLeft.add(lastModifiedLabel, gbc_lastModifiedLabel);

        lastModifiedDetails = new JXLabel();
        lastModifiedDetails.setText(" ");
        lastModifiedDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_lastModifiedDetails = new GridBagConstraints();
        gbc_lastModifiedDetails.anchor = GridBagConstraints.SOUTH;
        gbc_lastModifiedDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_lastModifiedDetails.gridx = 1;
        gbc_lastModifiedDetails.gridy = 6;
        detailsPanelLeft.add(lastModifiedDetails, gbc_lastModifiedDetails);

        JXPanel detailsPanelRight = new JXPanel();
        detailsPanelRight.setBorder(null);
        detailsPanelMain.add(detailsPanelRight, BorderLayout.CENTER);
        GridBagLayout gbl_detailsPanelRight = new GridBagLayout();
        gbl_detailsPanelRight.columnWidths = new int[]{0, 0, 0};
        gbl_detailsPanelRight.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelRight.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gbl_detailsPanelRight.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelRight.setLayout(gbl_detailsPanelRight);

        JXLabel teamLabel = new JXLabel();
        teamLabel.setText("Team");
        teamLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        teamLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_teamLabel = new GridBagConstraints();
        gbc_teamLabel.anchor = GridBagConstraints.WEST;
        gbc_teamLabel.insets = new Insets(0, 0, 5, 5);
        gbc_teamLabel.gridx = 0;
        gbc_teamLabel.gridy = 0;
        detailsPanelRight.add(teamLabel, gbc_teamLabel);

        teamDetails = new JXLabel();
        teamDetails.setText(" ");
        teamDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_teamDetails = new GridBagConstraints();
        gbc_teamDetails.anchor = GridBagConstraints.SOUTH;
        gbc_teamDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_teamDetails.insets = new Insets(0, 0, 5, 0);
        gbc_teamDetails.gridx = 1;
        gbc_teamDetails.gridy = 0;
        detailsPanelRight.add(teamDetails, gbc_teamDetails);

        JXLabel enviromentLabel = new JXLabel();
        GridBagConstraints gbc_enviromentLabel = new GridBagConstraints();
        gbc_enviromentLabel.anchor = GridBagConstraints.WEST;
        gbc_enviromentLabel.insets = new Insets(0, 0, 5, 5);
        gbc_enviromentLabel.gridx = 0;
        gbc_enviromentLabel.gridy = 1;
        detailsPanelRight.add(enviromentLabel, gbc_enviromentLabel);
        enviromentLabel.setText("Enviroment");
        enviromentLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        enviromentLabel.setBorder(new EmptyBorder(0, 30, 0, 10));

        enviromentDetails = new JXLabel();
        GridBagConstraints gbc_enviromentDetails = new GridBagConstraints();
        gbc_enviromentDetails.anchor = GridBagConstraints.SOUTH;
        gbc_enviromentDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_enviromentDetails.insets = new Insets(0, 0, 5, 0);
        gbc_enviromentDetails.gridx = 1;
        gbc_enviromentDetails.gridy = 1;
        detailsPanelRight.add(enviromentDetails, gbc_enviromentDetails);
        enviromentDetails.setText(" ");
        enviromentDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));

        JXLabel appModulesLabel = new JXLabel();
        appModulesLabel.setText("Application Module");
        appModulesLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        appModulesLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_appModulesLabel = new GridBagConstraints();
        gbc_appModulesLabel.anchor = GridBagConstraints.WEST;
        gbc_appModulesLabel.insets = new Insets(0, 0, 5, 5);
        gbc_appModulesLabel.gridx = 0;
        gbc_appModulesLabel.gridy = 2;
        detailsPanelRight.add(appModulesLabel, gbc_appModulesLabel);

        appModuleDetails = new JXLabel();
        appModuleDetails.setText(" ");
        appModuleDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_appModuleDetails = new GridBagConstraints();
        gbc_appModuleDetails.anchor = GridBagConstraints.SOUTH;
        gbc_appModuleDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_appModuleDetails.insets = new Insets(0, 0, 5, 0);
        gbc_appModuleDetails.gridx = 1;
        gbc_appModuleDetails.gridy = 2;
        detailsPanelRight.add(appModuleDetails, gbc_appModuleDetails);

        JXLabel itemOriginLabel = new JXLabel();
        itemOriginLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        itemOriginLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        itemOriginLabel.setText("Item origin");
        GridBagConstraints gbc_itemOriginLabel = new GridBagConstraints();
        gbc_itemOriginLabel.anchor = GridBagConstraints.WEST;
        gbc_itemOriginLabel.insets = new Insets(0, 0, 5, 5);
        gbc_itemOriginLabel.gridx = 0;
        gbc_itemOriginLabel.gridy = 3;
        detailsPanelRight.add(itemOriginLabel, gbc_itemOriginLabel);

        itemOriginDetails = new JXLabel();
        itemOriginDetails.setText(" ");
        itemOriginDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_itemOriginDetails = new GridBagConstraints();
        gbc_itemOriginDetails.anchor = GridBagConstraints.SOUTH;
        gbc_itemOriginDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_itemOriginDetails.insets = new Insets(0, 0, 5, 0);
        gbc_itemOriginDetails.gridx = 1;
        gbc_itemOriginDetails.gridy = 3;
        detailsPanelRight.add(itemOriginDetails, gbc_itemOriginDetails);

        JXLabel blockedReasonLabel = new JXLabel();
        blockedReasonLabel.setText("Blocked Reason");
        blockedReasonLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        blockedReasonLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_blockedReasonLabel = new GridBagConstraints();
        gbc_blockedReasonLabel.anchor = GridBagConstraints.WEST;
        gbc_blockedReasonLabel.insets = new Insets(0, 0, 5, 5);
        gbc_blockedReasonLabel.gridx = 0;
        gbc_blockedReasonLabel.gridy = 4;
        detailsPanelRight.add(blockedReasonLabel, gbc_blockedReasonLabel);

        blockedReasonDetails = new JXLabel();
        blockedReasonDetails.setText(" ");
        blockedReasonDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_blockedReasonDetails = new GridBagConstraints();
        gbc_blockedReasonDetails.anchor = GridBagConstraints.SOUTH;
        gbc_blockedReasonDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_blockedReasonDetails.insets = new Insets(0, 0, 5, 0);
        gbc_blockedReasonDetails.gridx = 1;
        gbc_blockedReasonDetails.gridy = 4;
        detailsPanelRight.add(blockedReasonDetails, gbc_blockedReasonDetails);

        JXLabel releaseLabel = new JXLabel();
        GridBagConstraints gbc_releaseLabel = new GridBagConstraints();
        gbc_releaseLabel.anchor = GridBagConstraints.WEST;
        gbc_releaseLabel.insets = new Insets(0, 0, 5, 5);
        gbc_releaseLabel.gridx = 0;
        gbc_releaseLabel.gridy = 5;
        detailsPanelRight.add(releaseLabel, gbc_releaseLabel);
        releaseLabel.setText("Release");
        releaseLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        releaseLabel.setBorder(new EmptyBorder(0, 30, 0, 10));

        releaseDetails = new JXLabel();
        GridBagConstraints gbc_releaseDetails = new GridBagConstraints();
        gbc_releaseDetails.anchor = GridBagConstraints.SOUTH;
        gbc_releaseDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_releaseDetails.insets = new Insets(0, 0, 5, 0);
        gbc_releaseDetails.gridx = 1;
        gbc_releaseDetails.gridy = 5;
        detailsPanelRight.add(releaseDetails, gbc_releaseDetails);
        releaseDetails.setText(" ");
        releaseDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));

        JXLabel creationTimeLabel = new JXLabel();
        creationTimeLabel.setText("Creation time");
        creationTimeLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        creationTimeLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_creationTimeLabel = new GridBagConstraints();
        gbc_creationTimeLabel.anchor = GridBagConstraints.WEST;
        gbc_creationTimeLabel.insets = new Insets(0, 0, 0, 5);
        gbc_creationTimeLabel.gridx = 0;
        gbc_creationTimeLabel.gridy = 6;
        detailsPanelRight.add(creationTimeLabel, gbc_creationTimeLabel);

        creationTimeDetails = new JXLabel();
        creationTimeDetails.setText(" ");
        creationTimeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_creationTimeDetails = new GridBagConstraints();
        gbc_creationTimeDetails.anchor = GridBagConstraints.SOUTH;
        gbc_creationTimeDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_creationTimeDetails.gridx = 1;
        gbc_creationTimeDetails.gridy = 6;
        detailsPanelRight.add(creationTimeDetails, gbc_creationTimeDetails);

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

    public void setSprintDetails(String sprintDetails) {
        this.sprintDetails.setText(sprintDetails);
    }

    public void setStoryPointsDetails(String storyPointsDetails) {
        this.storyPointsDetails.setText(storyPointsDetails);
    }

    public void setBlockedDetails(String blockedDetails) {
        this.blockedDetails.setText(blockedDetails);
    }

    public void setEnviromentDetails(String enviromentDetails) {
        this.enviromentDetails.setText(enviromentDetails);
    }

    public void setReleaseDetails(String releaseDetails) {
        this.releaseDetails.setText(releaseDetails);
    }

    public void setLastModifiedDetails(String lastModifiedDetails) {
        this.lastModifiedDetails.setText(lastModifiedDetails);
    }

    public void setTeamDetails(String teamDetails) {
        this.teamDetails.setText(teamDetails);
    }

    public void setBlockedReasonDetails(String blockedReasonDetails) {
        this.blockedReasonDetails.setText(blockedReasonDetails);
    }

    public void setAppModuleDetails(String appModuleDetails) {
        this.appModuleDetails.setText(appModuleDetails);
    }

    public void setCreationTimeDetails(String creationTimeDetails) {
        this.creationTimeDetails.setText(creationTimeDetails);
    }

    public void setLastRunsDetails(String lastRunsDetails) {
        this.lastRunsDetails.setText(lastRunsDetails);
    }

    public void setItemOriginDetails(String itemOriginDetails) {
        this.itemOriginDetails.setText(itemOriginDetails);
    }
}
