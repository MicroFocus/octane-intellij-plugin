package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import org.jdesktop.swingx.JXLabel.TextAlignment;

public class ManualTestRunDetailsPanel extends JXPanel {
    private static final long serialVersionUID = -7172388625845199450L;
    private JXLabel startedTimeDetails;
    private JXLabel testNameDetails;
    private JXLabel runByDetails;
    private JXLabel authorDetails;
    private JXLabel contentDetails;
    private JXLabel releaseDetails;
    private JXLabel nativeStatusDetails;
    private JXLabel durationDetails;
    private JXLabel draftRunDetails;
    private JXLabel lastModifiedDetails;
    private JXLabel environmentDetails;
    private JXLabel versionFromReleaseDescription;


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
        gbl_detailsPanelLeft.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelLeft.columnWeights = new double[]{0.0, 1.0};
        gbl_detailsPanelLeft.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
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

        runByDetails = new JXLabel();
        runByDetails.setText(" ");
        runByDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_runByDetails = new GridBagConstraints();
        gbc_runByDetails.anchor = GridBagConstraints.SOUTH;
        gbc_runByDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_runByDetails.insets = new Insets(0, 0, 5, 0);
        gbc_runByDetails.gridx = 1;
        gbc_runByDetails.gridy = 1;
        detailsPanelLeft.add(runByDetails, gbc_runByDetails);

        JXLabel startedDateLabel = new JXLabel();
        GridBagConstraints gbc_startedDateLabel = new GridBagConstraints();
        gbc_startedDateLabel.anchor = GridBagConstraints.WEST;
        gbc_startedDateLabel.insets = new Insets(0, 0, 5, 5);
        gbc_startedDateLabel.gridx = 0;
        gbc_startedDateLabel.gridy = 2;
        detailsPanelLeft.add(startedDateLabel, gbc_startedDateLabel);
        startedDateLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        startedDateLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        startedDateLabel.setText("Started");

        startedTimeDetails = new JXLabel();
        GridBagConstraints gbc_startedTimeDetails = new GridBagConstraints();
        gbc_startedTimeDetails.anchor = GridBagConstraints.SOUTH;
        gbc_startedTimeDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_startedTimeDetails.insets = new Insets(0, 0, 5, 0);
        gbc_startedTimeDetails.gridx = 1;
        gbc_startedTimeDetails.gridy = 2;
        detailsPanelLeft.add(startedTimeDetails, gbc_startedTimeDetails);
        startedTimeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        startedTimeDetails.setText(" ");

        JXLabel contentLabel = new JXLabel();
        contentLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        contentLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        contentLabel.setText("Content");
        GridBagConstraints gbc_contentLabel = new GridBagConstraints();
        gbc_contentLabel.anchor = GridBagConstraints.WEST;
        gbc_contentLabel.insets = new Insets(0, 0, 5, 5);
        gbc_contentLabel.gridx = 0;
        gbc_contentLabel.gridy = 3;
        detailsPanelLeft.add(contentLabel, gbc_contentLabel);

        contentDetails = new JXLabel();
        contentDetails.setText(" ");
        contentDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_contentDetails = new GridBagConstraints();
        gbc_contentDetails.anchor = GridBagConstraints.SOUTH;
        gbc_contentDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_contentDetails.insets = new Insets(0, 0, 5, 0);
        gbc_contentDetails.gridx = 1;
        gbc_contentDetails.gridy = 3;
        detailsPanelLeft.add(contentDetails, gbc_contentDetails);

        JXLabel releaseLabel = new JXLabel();
        releaseLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        releaseLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        releaseLabel.setText("Release");
        GridBagConstraints gbc_releaseLabel = new GridBagConstraints();
        gbc_releaseLabel.anchor = GridBagConstraints.WEST;
        gbc_releaseLabel.insets = new Insets(0, 0, 5, 5);
        gbc_releaseLabel.gridx = 0;
        gbc_releaseLabel.gridy = 4;
        detailsPanelLeft.add(releaseLabel, gbc_releaseLabel);

        releaseDetails = new JXLabel();
        releaseDetails.setText(" ");
        releaseDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_releaseDetails = new GridBagConstraints();
        gbc_releaseDetails.insets = new Insets(0, 0, 5, 0);
        gbc_releaseDetails.anchor = GridBagConstraints.SOUTH;
        gbc_releaseDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_releaseDetails.gridx = 1;
        gbc_releaseDetails.gridy = 4;
        detailsPanelLeft.add(releaseDetails, gbc_releaseDetails);
        
        JXLabel environmentLabel = new JXLabel();
        environmentLabel.setText("Environment");
        environmentLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        environmentLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_environmentLabel = new GridBagConstraints();
        gbc_environmentLabel.anchor = GridBagConstraints.WEST;
        gbc_environmentLabel.insets = new Insets(0, 0, 0, 5);
        gbc_environmentLabel.gridx = 0;
        gbc_environmentLabel.gridy = 5;
        detailsPanelLeft.add(environmentLabel, gbc_environmentLabel);
        
        environmentDetails = new JXLabel();
        environmentDetails.setText(" ");
        environmentDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_environmentDetails = new GridBagConstraints();
        gbc_environmentDetails.anchor = GridBagConstraints.SOUTH;
        gbc_environmentDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_environmentDetails.gridx = 1;
        gbc_environmentDetails.gridy = 5;
        detailsPanelLeft.add(environmentDetails, gbc_environmentDetails);

        JXPanel detailsPanelRight = new JXPanel();
        detailsPanelRight.setBorder(null);
        detailsPanelMain.add(detailsPanelRight, BorderLayout.CENTER);
        GridBagLayout gbl_detailsPanelRight = new GridBagLayout();
        gbl_detailsPanelRight.columnWidths = new int[]{0, 0, 0};
        gbl_detailsPanelRight.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelRight.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gbl_detailsPanelRight.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelRight.setLayout(gbl_detailsPanelRight);

        JXLabel nativeStatusLabel = new JXLabel();
        nativeStatusLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        nativeStatusLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        nativeStatusLabel.setText("Native status");
        GridBagConstraints gbc_nativeStatusLabel = new GridBagConstraints();
        gbc_nativeStatusLabel.anchor = GridBagConstraints.WEST;
        gbc_nativeStatusLabel.insets = new Insets(0, 0, 5, 5);
        gbc_nativeStatusLabel.gridx = 0;
        gbc_nativeStatusLabel.gridy = 0;
        detailsPanelRight.add(nativeStatusLabel, gbc_nativeStatusLabel);

        nativeStatusDetails = new JXLabel();
        nativeStatusDetails.setText(" ");
        nativeStatusDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_nativeStatusDetails = new GridBagConstraints();
        gbc_nativeStatusDetails.anchor = GridBagConstraints.SOUTH;
        gbc_nativeStatusDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_nativeStatusDetails.insets = new Insets(0, 0, 5, 0);
        gbc_nativeStatusDetails.gridx = 1;
        gbc_nativeStatusDetails.gridy = 0;
        detailsPanelRight.add(nativeStatusDetails, gbc_nativeStatusDetails);

        JXLabel authorLabel = new JXLabel();
        GridBagConstraints gbc_authorLabel = new GridBagConstraints();
        gbc_authorLabel.anchor = GridBagConstraints.WEST;
        gbc_authorLabel.insets = new Insets(0, 0, 5, 5);
        gbc_authorLabel.gridx = 0;
        gbc_authorLabel.gridy = 1;
        detailsPanelRight.add(authorLabel, gbc_authorLabel);
        authorLabel.setText("Author");
        authorLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        authorLabel.setBorder(new EmptyBorder(0, 30, 0, 10));

        authorDetails = new JXLabel();
        GridBagConstraints gbc_authorDetails = new GridBagConstraints();
        gbc_authorDetails.anchor = GridBagConstraints.SOUTH;
        gbc_authorDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_authorDetails.insets = new Insets(0, 0, 5, 0);
        gbc_authorDetails.gridx = 1;
        gbc_authorDetails.gridy = 1;
        detailsPanelRight.add(authorDetails, gbc_authorDetails);
        authorDetails.setText(" ");
        authorDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));

        JXLabel durationLabel = new JXLabel();
        durationLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        durationLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        durationLabel.setText("Duration");
        GridBagConstraints gbc_durationLabel = new GridBagConstraints();
        gbc_durationLabel.anchor = GridBagConstraints.WEST;
        gbc_durationLabel.insets = new Insets(0, 0, 5, 5);
        gbc_durationLabel.gridx = 0;
        gbc_durationLabel.gridy = 2;
        detailsPanelRight.add(durationLabel, gbc_durationLabel);

        durationDetails = new JXLabel();
        durationDetails.setText(" ");
        durationDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_durationDetails = new GridBagConstraints();
        gbc_durationDetails.anchor = GridBagConstraints.SOUTH;
        gbc_durationDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_durationDetails.insets = new Insets(0, 0, 5, 0);
        gbc_durationDetails.gridx = 1;
        gbc_durationDetails.gridy = 2;
        detailsPanelRight.add(durationDetails, gbc_durationDetails);

        JXLabel draftRunLabel = new JXLabel();
        draftRunLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        draftRunLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        draftRunLabel.setText("Draft run");
        GridBagConstraints gbc_draftRunLabel = new GridBagConstraints();
        gbc_draftRunLabel.anchor = GridBagConstraints.WEST;
        gbc_draftRunLabel.insets = new Insets(0, 0, 5, 5);
        gbc_draftRunLabel.gridx = 0;
        gbc_draftRunLabel.gridy = 3;
        detailsPanelRight.add(draftRunLabel, gbc_draftRunLabel);

        draftRunDetails = new JXLabel();
        draftRunDetails.setText(" ");
        draftRunDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_draftRunDetails = new GridBagConstraints();
        gbc_draftRunDetails.insets = new Insets(0, 0, 5, 0);
        gbc_draftRunDetails.anchor = GridBagConstraints.SOUTH;
        gbc_draftRunDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_draftRunDetails.gridx = 1;
        gbc_draftRunDetails.gridy = 3;
        detailsPanelRight.add(draftRunDetails, gbc_draftRunDetails);
        
        JXLabel versionFromReleaseLabel = new JXLabel();
        versionFromReleaseLabel.setText("Version from release");
        versionFromReleaseLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        versionFromReleaseLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_versionFromReleaseLabel = new GridBagConstraints();
        gbc_versionFromReleaseLabel.anchor = GridBagConstraints.WEST;
        gbc_versionFromReleaseLabel.insets = new Insets(0, 0, 5, 5);
        gbc_versionFromReleaseLabel.gridx = 0;
        gbc_versionFromReleaseLabel.gridy = 4;
        detailsPanelRight.add(versionFromReleaseLabel, gbc_versionFromReleaseLabel);
        
        versionFromReleaseDescription = new JXLabel();
        versionFromReleaseDescription.setText(" ");
        versionFromReleaseDescription.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_versionFromReleaseDescription = new GridBagConstraints();
        gbc_versionFromReleaseDescription.anchor = GridBagConstraints.SOUTH;
        gbc_versionFromReleaseDescription.fill = GridBagConstraints.HORIZONTAL;
        gbc_versionFromReleaseDescription.insets = new Insets(0, 0, 5, 0);
        gbc_versionFromReleaseDescription.gridx = 1;
        gbc_versionFromReleaseDescription.gridy = 4;
        detailsPanelRight.add(versionFromReleaseDescription, gbc_versionFromReleaseDescription);
        
        JXLabel lastModifiedLabel = new JXLabel();
        lastModifiedLabel.setText("Last modified");
        lastModifiedLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        lastModifiedLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        GridBagConstraints gbc_lastModifiedLabel = new GridBagConstraints();
        gbc_lastModifiedLabel.anchor = GridBagConstraints.WEST;
        gbc_lastModifiedLabel.insets = new Insets(0, 0, 0, 5);
        gbc_lastModifiedLabel.gridx = 0;
        gbc_lastModifiedLabel.gridy = 5;
        detailsPanelRight.add(lastModifiedLabel, gbc_lastModifiedLabel);
        
        lastModifiedDetails = new JXLabel();
        lastModifiedDetails.setText(" ");
        lastModifiedDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_lastModifiedDetails = new GridBagConstraints();
        gbc_lastModifiedDetails.anchor = GridBagConstraints.SOUTH;
        gbc_lastModifiedDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_lastModifiedDetails.gridx = 1;
        gbc_lastModifiedDetails.gridy = 5;
        detailsPanelRight.add(lastModifiedDetails, gbc_lastModifiedDetails);

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

    public void setStartedTimeDetails(String startedTimeDetails) {
		this.startedTimeDetails.setText(startedTimeDetails);
	}
    public void setTestNameDetails(String testNameDetails) {
        this.testNameDetails.setText(testNameDetails);
    }
    public void setRunByDetails(String runByDetails) {
        this.runByDetails.setText(runByDetails);
    }
    public void setAuthorDetails(String authorDetails) {
        this.authorDetails.setText(authorDetails);
    }
    public void setContentDetails(String contentDetails) {
        this.contentDetails.setText(contentDetails);
    }
    public void setReleaseDetails(String releaseDetails) {
        this.releaseDetails.setText(releaseDetails);
    }
    public void setNativeStatusDetails(String nativeStatusDetails) {
        this.nativeStatusDetails.setText(nativeStatusDetails);
    }
    public void setDurationDetails(String durationDetails) {
        this.durationDetails.setText(durationDetails);
    }
    public void setDraftRunDetails(String draftRunDetails) {
        this.draftRunDetails.setText(draftRunDetails);
    }
    public void setLastModifiedDetails(String lastModifiedDetails) {
		this.lastModifiedDetails.setText(lastModifiedDetails);
	}
	public void setEnvironmentDetails(String environmentDetails) {
		this.environmentDetails.setText(environmentDetails);
	}
	public void setVersionFromReleaseDescription(String versionFromReleaseDescription) {
		this.versionFromReleaseDescription.setText(versionFromReleaseDescription);;
	}
	
}
