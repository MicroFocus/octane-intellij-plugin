package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

public class TaskDetailsPanel extends JXPanel {
    private static final long serialVersionUID = -7172388625845199450L;
    private JXLabel ownerDetails;
    private JXLabel storyDetails;
    private JXLabel authorDetails;
    private JXLabel sprintDetails;
    private JXLabel creationTimeDetails;
    private JXLabel lastModifiedDetails;
    private JXLabel taskTypeDetails;
    private JXLabel estimatedHoursDetails;
    private JXLabel investedHoursDetails;


    public TaskDetailsPanel() {
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
        gbl_detailsPanelLeft.rowHeights = new int[]{0, 0, 0, 0, 0, 0};
        gbl_detailsPanelLeft.columnWeights = new double[]{0.0, 1.0};
        gbl_detailsPanelLeft.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelLeft.setLayout(gbl_detailsPanelLeft);

        JXLabel storyLabel = new JXLabel();
        storyLabel.setText("Story");
        storyLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        storyLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_storyLabel = new GridBagConstraints();
        gbc_storyLabel.anchor = GridBagConstraints.WEST;
        gbc_storyLabel.insets = new Insets(0, 0, 5, 5);
        gbc_storyLabel.gridx = 0;
        gbc_storyLabel.gridy = 0;
        detailsPanelLeft.add(storyLabel, gbc_storyLabel);

        storyDetails = new JXLabel();
        storyDetails.setText("                                          ");
        storyDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_storyDetails = new GridBagConstraints();
        gbc_storyDetails.anchor = GridBagConstraints.SOUTH;
        gbc_storyDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_storyDetails.insets = new Insets(0, 0, 5, 0);
        gbc_storyDetails.gridx = 1;
        gbc_storyDetails.gridy = 0;
        detailsPanelLeft.add(storyDetails, gbc_storyDetails);

        JXLabel authorLabel = new JXLabel();
        authorLabel.setText("Author");
        authorLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        authorLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_authorLabel = new GridBagConstraints();
        gbc_authorLabel.anchor = GridBagConstraints.WEST;
        gbc_authorLabel.insets = new Insets(0, 0, 5, 5);
        gbc_authorLabel.gridx = 0;
        gbc_authorLabel.gridy = 1;
        detailsPanelLeft.add(authorLabel, gbc_authorLabel);

        authorDetails = new JXLabel();
        authorDetails.setText(" ");
        authorDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_authorDetails = new GridBagConstraints();
        gbc_authorDetails.anchor = GridBagConstraints.SOUTH;
        gbc_authorDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_authorDetails.insets = new Insets(0, 0, 5, 0);
        gbc_authorDetails.gridx = 1;
        gbc_authorDetails.gridy = 1;
        detailsPanelLeft.add(authorDetails, gbc_authorDetails);

        JXLabel ownerLabel = new JXLabel();
        GridBagConstraints gbc_ownerLabel = new GridBagConstraints();
        gbc_ownerLabel.anchor = GridBagConstraints.WEST;
        gbc_ownerLabel.insets = new Insets(0, 0, 5, 5);
        gbc_ownerLabel.gridx = 0;
        gbc_ownerLabel.gridy = 2;
        detailsPanelLeft.add(ownerLabel, gbc_ownerLabel);
        ownerLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        ownerLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        ownerLabel.setText("Owner");

        ownerDetails = new JXLabel();
        GridBagConstraints gbc_ownerDetails = new GridBagConstraints();
        gbc_ownerDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_ownerDetails.insets = new Insets(0, 0, 5, 0);
        gbc_ownerDetails.gridx = 1;
        gbc_ownerDetails.gridy = 2;
        detailsPanelLeft.add(ownerDetails, gbc_ownerDetails);
        ownerDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        ownerDetails.setText(" ");

        JXLabel creationTimeLabel = new JXLabel();
        creationTimeLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        creationTimeLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        creationTimeLabel.setText("Creation time");
        GridBagConstraints gbc_creationTimeLabel = new GridBagConstraints();
        gbc_creationTimeLabel.anchor = GridBagConstraints.WEST;
        gbc_creationTimeLabel.insets = new Insets(0, 0, 5, 5);
        gbc_creationTimeLabel.gridx = 0;
        gbc_creationTimeLabel.gridy = 3;
        detailsPanelLeft.add(creationTimeLabel, gbc_creationTimeLabel);

        creationTimeDetails = new JXLabel();
        creationTimeDetails.setText(" ");
        creationTimeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_creationTimeDetails = new GridBagConstraints();
        gbc_creationTimeDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_creationTimeDetails.insets = new Insets(0, 0, 5, 0);
        gbc_creationTimeDetails.gridx = 1;
        gbc_creationTimeDetails.gridy = 3;
        detailsPanelLeft.add(creationTimeDetails, gbc_creationTimeDetails);

        JXLabel lastModifiedLabel = new JXLabel();
        lastModifiedLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        lastModifiedLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        lastModifiedLabel.setText("Last modified");
        GridBagConstraints gbc_lastModifiedLabel = new GridBagConstraints();
        gbc_lastModifiedLabel.anchor = GridBagConstraints.WEST;
        gbc_lastModifiedLabel.insets = new Insets(0, 0, 0, 5);
        gbc_lastModifiedLabel.gridx = 0;
        gbc_lastModifiedLabel.gridy = 4;
        detailsPanelLeft.add(lastModifiedLabel, gbc_lastModifiedLabel);

        lastModifiedDetails = new JXLabel();
        lastModifiedDetails.setText(" ");
        lastModifiedDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_lastModifiedDetails = new GridBagConstraints();
        gbc_lastModifiedDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_lastModifiedDetails.gridx = 1;
        gbc_lastModifiedDetails.gridy = 4;
        detailsPanelLeft.add(lastModifiedDetails, gbc_lastModifiedDetails);

        JXPanel detailsPanelRight = new JXPanel();
        detailsPanelRight.setBorder(null);
        detailsPanelMain.add(detailsPanelRight, BorderLayout.CENTER);
        GridBagLayout gbl_detailsPanelRight = new GridBagLayout();
        gbl_detailsPanelRight.columnWidths = new int[]{0, 0, 0};
        gbl_detailsPanelRight.rowHeights = new int[]{0, 0, 0, 0, 0};
        gbl_detailsPanelRight.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gbl_detailsPanelRight.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelRight.setLayout(gbl_detailsPanelRight);

        JXLabel taskTypeLabel = new JXLabel();
        taskTypeLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        taskTypeLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        taskTypeLabel.setText("Type");
        GridBagConstraints gbc_taskTypeLabel = new GridBagConstraints();
        gbc_taskTypeLabel.anchor = GridBagConstraints.WEST;
        gbc_taskTypeLabel.insets = new Insets(0, 0, 5, 5);
        gbc_taskTypeLabel.gridx = 0;
        gbc_taskTypeLabel.gridy = 0;
        detailsPanelRight.add(taskTypeLabel, gbc_taskTypeLabel);

        taskTypeDetails = new JXLabel();
        taskTypeDetails.setText(" ");
        taskTypeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_taskTypeDetails = new GridBagConstraints();
        gbc_taskTypeDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_taskTypeDetails.insets = new Insets(0, 0, 5, 0);
        gbc_taskTypeDetails.gridx = 1;
        gbc_taskTypeDetails.gridy = 0;
        detailsPanelRight.add(taskTypeDetails, gbc_taskTypeDetails);

        JXLabel sprintLabel = new JXLabel();
        GridBagConstraints gbc_sprintLabel = new GridBagConstraints();
        gbc_sprintLabel.anchor = GridBagConstraints.WEST;
        gbc_sprintLabel.insets = new Insets(0, 0, 5, 5);
        gbc_sprintLabel.gridx = 0;
        gbc_sprintLabel.gridy = 1;
        detailsPanelRight.add(sprintLabel, gbc_sprintLabel);
        sprintLabel.setText("Sprint");
        sprintLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        sprintLabel.setBorder(new EmptyBorder(0, 30, 0, 10));

        sprintDetails = new JXLabel();
        GridBagConstraints gbc_sprintDetails = new GridBagConstraints();
        gbc_sprintDetails.anchor = GridBagConstraints.SOUTH;
        gbc_sprintDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_sprintDetails.insets = new Insets(0, 0, 5, 0);
        gbc_sprintDetails.gridx = 1;
        gbc_sprintDetails.gridy = 1;
        detailsPanelRight.add(sprintDetails, gbc_sprintDetails);
        sprintDetails.setText(" ");
        sprintDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));

        JXLabel estimatedHoursLabel = new JXLabel();
        estimatedHoursLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        estimatedHoursLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        estimatedHoursLabel.setText("Estimated hours");
        GridBagConstraints gbc_estimatedHoursLabel = new GridBagConstraints();
        gbc_estimatedHoursLabel.anchor = GridBagConstraints.WEST;
        gbc_estimatedHoursLabel.insets = new Insets(0, 0, 5, 5);
        gbc_estimatedHoursLabel.gridx = 0;
        gbc_estimatedHoursLabel.gridy = 2;
        detailsPanelRight.add(estimatedHoursLabel, gbc_estimatedHoursLabel);

        estimatedHoursDetails = new JXLabel();
        estimatedHoursDetails.setText(" ");
        estimatedHoursDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_estimatedHoursDetails = new GridBagConstraints();
        gbc_estimatedHoursDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_estimatedHoursDetails.insets = new Insets(0, 0, 5, 0);
        gbc_estimatedHoursDetails.gridx = 1;
        gbc_estimatedHoursDetails.gridy = 2;
        detailsPanelRight.add(estimatedHoursDetails, gbc_estimatedHoursDetails);

        JXLabel investedHoursLabel = new JXLabel();
        investedHoursLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        investedHoursLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        investedHoursLabel.setText("Invested hours");
        GridBagConstraints gbc_investedHoursLabel = new GridBagConstraints();
        gbc_investedHoursLabel.anchor = GridBagConstraints.WEST;
        gbc_investedHoursLabel.insets = new Insets(0, 0, 0, 5);
        gbc_investedHoursLabel.gridx = 0;
        gbc_investedHoursLabel.gridy = 3;
        detailsPanelRight.add(investedHoursLabel, gbc_investedHoursLabel);

        investedHoursDetails = new JXLabel();
        investedHoursDetails.setText(" ");
        investedHoursDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_investedHoursDetails = new GridBagConstraints();
        gbc_investedHoursDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_investedHoursDetails.gridx = 1;
        gbc_investedHoursDetails.gridy = 3;
        detailsPanelRight.add(investedHoursDetails, gbc_investedHoursDetails);

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


    public void setStoryDetails(String featureDetails) {
        this.storyDetails.setText(featureDetails);
    }


    public void setAuthorDetails(String severityDetails) {
        this.authorDetails.setText(severityDetails);
    }


    public void setSprintDetails(String sprintDetails) {
        this.sprintDetails.setText(sprintDetails);
    }


    public void setCreationTimeDetails(String creationTypeDetails) {
        this.creationTimeDetails.setText(creationTypeDetails);
    }


    public void setLastModifiedDetails(String lastModifiedDetails) {
        this.lastModifiedDetails.setText(lastModifiedDetails);
    }


    public void setTaskTypeDetails(String taskTypeDetails) {
        this.taskTypeDetails.setText(taskTypeDetails);
    }


    public void setEstimatedHoursDetails(String estimatedHoursDetails) {
        this.estimatedHoursDetails.setText(estimatedHoursDetails);
    }


    public void setInvestedHoursDetails(String investedHoursDetails) {
        this.investedHoursDetails.setText(investedHoursDetails);
    }

}
