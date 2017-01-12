package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

public class ManualTestRunDetailsPanel extends JXPanel {
    private static final long serialVersionUID = -7172388625845199450L;
    private JXLabel ownerDetails;
    private JXLabel testNameDetails;
    private JXLabel authorDetails;
    private JXLabel remainingHoursDetails;
    private JXLabel creationTimeDetails;
    private JXLabel lastModifiedDetails;
    private JXLabel taskTypeDetails;
    private JXLabel estimatedHoursDetails;
    private JXLabel investedHoursDetails;


    public ManualTestRunDetailsPanel() {
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

        JXLabel testNameLabel = new JXLabel();
        testNameLabel.setText("Test Name");
        testNameLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        testNameLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_testNameLabel = new GridBagConstraints();
        gbc_testNameLabel.anchor = GridBagConstraints.WEST;
        gbc_testNameLabel.insets = new Insets(0, 0, 5, 5);
        gbc_testNameLabel.gridx = 0;
        gbc_testNameLabel.gridy = 0;
        detailsPanelLeft.add(testNameLabel, gbc_testNameLabel);

        testNameDetails = new JXLabel();
        testNameDetails.setText("                                          ");
        testNameDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_testNameDetails = new GridBagConstraints();
        gbc_testNameDetails.anchor = GridBagConstraints.SOUTH;
        gbc_testNameDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_testNameDetails.insets = new Insets(0, 0, 5, 0);
        gbc_testNameDetails.gridx = 1;
        gbc_testNameDetails.gridy = 0;
        detailsPanelLeft.add(testNameDetails, gbc_testNameDetails);

        JXLabel runByLabel = new JXLabel();
        runByLabel.setText("Run by");
        runByLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        runByLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_runByLabel = new GridBagConstraints();
        gbc_runByLabel.anchor = GridBagConstraints.WEST;
        gbc_runByLabel.insets = new Insets(0, 0, 5, 5);
        gbc_runByLabel.gridx = 0;
        gbc_runByLabel.gridy = 1;
        detailsPanelLeft.add(runByLabel, gbc_runByLabel);

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
        gbc_ownerDetails.anchor = GridBagConstraints.SOUTH;
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
        gbc_creationTimeDetails.anchor = GridBagConstraints.SOUTH;
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
        gbc_lastModifiedDetails.anchor = GridBagConstraints.SOUTH;
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
        gbc_taskTypeDetails.anchor = GridBagConstraints.SOUTH;
        gbc_taskTypeDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_taskTypeDetails.insets = new Insets(0, 0, 5, 0);
        gbc_taskTypeDetails.gridx = 1;
        gbc_taskTypeDetails.gridy = 0;
        detailsPanelRight.add(taskTypeDetails, gbc_taskTypeDetails);

        JXLabel remainingHoursLabel = new JXLabel();
        GridBagConstraints gbc_remainingHoursLabel = new GridBagConstraints();
        gbc_remainingHoursLabel.anchor = GridBagConstraints.WEST;
        gbc_remainingHoursLabel.insets = new Insets(0, 0, 5, 5);
        gbc_remainingHoursLabel.gridx = 0;
        gbc_remainingHoursLabel.gridy = 1;
        detailsPanelRight.add(remainingHoursLabel, gbc_remainingHoursLabel);
        remainingHoursLabel.setText("Remaining hours");
        remainingHoursLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        remainingHoursLabel.setBorder(new EmptyBorder(0, 30, 0, 10));

        remainingHoursDetails = new JXLabel();
        GridBagConstraints gbc_remainingHoursDetails = new GridBagConstraints();
        gbc_remainingHoursDetails.anchor = GridBagConstraints.SOUTH;
        gbc_remainingHoursDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_remainingHoursDetails.insets = new Insets(0, 0, 5, 0);
        gbc_remainingHoursDetails.gridx = 1;
        gbc_remainingHoursDetails.gridy = 1;
        detailsPanelRight.add(remainingHoursDetails, gbc_remainingHoursDetails);
        remainingHoursDetails.setText(" ");
        remainingHoursDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));

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
        gbc_estimatedHoursDetails.anchor = GridBagConstraints.SOUTH;
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
        gbc_investedHoursDetails.anchor = GridBagConstraints.SOUTH;
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
        this.testNameDetails.setText(featureDetails);
    }
    public void setAuthorDetails(String severityDetails) {
        this.authorDetails.setText(severityDetails);
    }
    public void setRemainingHoursDetails(String remainingHoursDetails) {
        this.remainingHoursDetails.setText(remainingHoursDetails);
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
