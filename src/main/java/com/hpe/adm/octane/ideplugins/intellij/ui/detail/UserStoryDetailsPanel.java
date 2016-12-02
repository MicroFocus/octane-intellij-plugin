package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextArea;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;

public class UserStoryDetailsPanel extends JPanel {

    private JXPanel detailsPanel;

    private JXLabel lblDescription;
    private JXTextArea txtfldDescription;

    private JXLabel lblFeature;
    private JXTextField txtfldFeature;

    private JXLabel lblPhase;
    private JComboBox comboBoxPhase;
    private JXLabel lblTeam;
    private JXTextField txtfldTeam;
    private JXLabel lblGroup;
    private JXTextField txtfldGroup;
    private JXLabel lblQaOwner;
    private JXTextField txtfldQaowner;
    private JXLabel lblOwner;
    private JXTextField txtfldOwner;
    private JXLabel lblRelease;
    private JXLabel lblSprint;
    private JXTextField txtfldRelease;
    private JXTextField txtfldSprint;
    private JXLabel lblStoryPoints;
    private JXLabel lblAuthor;
    private JXTextField txtfldAuthor;
    private JXTextField txtfldStorypoints;
    private JXLabel lblQaStoryPoints;
    private JXTextField txtfldQastorypointsdays;
    private JXLabel lblDevStoryPoints;
    private JXTextField txtfldDevstorypointsdays;
    private JXLabel lblName;

    public UserStoryDetailsPanel() {
        setBounds(100, 100, 918, 336);
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0};
        gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{1.0, Double.MIN_VALUE};
        this.setLayout(gridBagLayout);

        JScrollPane scrollPane = new JScrollPane();
        GridBagConstraints gbc_scrollPane = new GridBagConstraints();
        gbc_scrollPane.fill = GridBagConstraints.BOTH;
        gbc_scrollPane.gridx = 0;
        gbc_scrollPane.gridy = 0;
        this.add(scrollPane, gbc_scrollPane);

        JXPanel parentPanel = new JXPanel();
        scrollPane.setViewportView(parentPanel);
        GridBagLayout gbl_parentPanel = new GridBagLayout();
        gbl_parentPanel.columnWidths = new int[]{0, 0};
        gbl_parentPanel.rowHeights = new int[]{0, 0, 0, 0, 0};
        gbl_parentPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_parentPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
        parentPanel.setLayout(gbl_parentPanel);

        lblName = new JXLabel();
        lblName.setFont(new Font("Tahoma", Font.BOLD | Font.ITALIC, 13));
        lblName.setBorder(new EmptyBorder(5, 10, 0, 10));
        lblName.setText("Name: ");
        GridBagConstraints gbc_lblName = new GridBagConstraints();
        gbc_lblName.anchor = GridBagConstraints.WEST;
        gbc_lblName.insets = new Insets(0, 0, 5, 0);
        gbc_lblName.gridx = 0;
        gbc_lblName.gridy = 0;
        parentPanel.add(lblName, gbc_lblName);

        lblDescription = new JXLabel();
        lblDescription.setBorder(new EmptyBorder(5, 10, 0, 10));
        lblDescription.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDescription.setText("Description");
        GridBagConstraints gbc_lblDescription = new GridBagConstraints();
        gbc_lblDescription.anchor = GridBagConstraints.NORTHWEST;
        gbc_lblDescription.insets = new Insets(0, 0, 5, 0);
        gbc_lblDescription.gridx = 0;
        gbc_lblDescription.gridy = 1;
        parentPanel.add(lblDescription, gbc_lblDescription);

        txtfldDescription = new JXTextArea();
        txtfldDescription.setEditable(false);
        txtfldDescription.setOpaque(false);
        txtfldDescription.setBackground(new Color(0, 0, 0, 0));
        txtfldDescription.setBorder(new EmptyBorder(5, 10, 0, 10));
        txtfldDescription.setLineWrap(true);
        txtfldDescription.setText("Description");
        GridBagConstraints gbc_txtrDescription = new GridBagConstraints();
        gbc_txtrDescription.insets = new Insets(0, 0, 5, 0);
        gbc_txtrDescription.fill = GridBagConstraints.BOTH;
        gbc_txtrDescription.gridx = 0;
        gbc_txtrDescription.gridy = 2;
        parentPanel.add(txtfldDescription, gbc_txtrDescription);

        detailsPanel = new JXPanel();
        detailsPanel.setBorder(new EmptyBorder(10, 10, 2, 10));
        GridBagConstraints gbc_detailsPanel = new GridBagConstraints();
        gbc_detailsPanel.fill = GridBagConstraints.BOTH;
        gbc_detailsPanel.gridx = 0;
        gbc_detailsPanel.gridy = 3;
        parentPanel.add(detailsPanel, gbc_detailsPanel);
        GridBagLayout gbl_detailsPanel = new GridBagLayout();
        gbl_detailsPanel.columnWidths = new int[]{0, 0, 0, 0, 0};
        gbl_detailsPanel.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanel.columnWeights = new double[]{0.0, 1.0, 0.0, 1.0, Double.MIN_VALUE};
        gbl_detailsPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanel.setLayout(gbl_detailsPanel);

        lblTeam = new JXLabel();
        lblTeam.setText("Team");
        lblTeam.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblTeam.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblTeam = new GridBagConstraints();
        gbc_lblTeam.anchor = GridBagConstraints.WEST;
        gbc_lblTeam.insets = new Insets(0, 0, 5, 5);
        gbc_lblTeam.gridx = 0;
        gbc_lblTeam.gridy = 0;
        detailsPanel.add(lblTeam, gbc_lblTeam);

        txtfldTeam = new JXTextField();
        txtfldTeam.setEditable(false);
        txtfldTeam.setOpaque(false);
        txtfldTeam.setBackground(new Color(0, 0, 0, 0));
        txtfldTeam.setBackground(new Color(0, 0, 0, 0));
        txtfldTeam.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldTeam = new GridBagConstraints();
        gbc_txtfldTeam.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldTeam.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldTeam.gridx = 1;
        gbc_txtfldTeam.gridy = 0;
        detailsPanel.add(txtfldTeam, gbc_txtfldTeam);

        lblGroup = new JXLabel();
        lblGroup.setText("Group");
        lblGroup.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblGroup.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblGroup = new GridBagConstraints();
        gbc_lblGroup.anchor = GridBagConstraints.WEST;
        gbc_lblGroup.insets = new Insets(0, 0, 5, 5);
        gbc_lblGroup.gridx = 2;
        gbc_lblGroup.gridy = 0;
        detailsPanel.add(lblGroup, gbc_lblGroup);

        txtfldGroup = new JXTextField();
        txtfldGroup.setEditable(false);
        txtfldGroup.setOpaque(false);
        txtfldGroup.setBackground(new Color(0, 0, 0, 0));
        txtfldGroup.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldGroup = new GridBagConstraints();
        gbc_txtfldGroup.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldGroup.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldGroup.gridx = 3;
        gbc_txtfldGroup.gridy = 0;
        detailsPanel.add(txtfldGroup, gbc_txtfldGroup);

        lblQaOwner = new JXLabel();
        lblQaOwner.setText("QA Owner");
        lblQaOwner.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblQaOwner.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblQaOwner = new GridBagConstraints();
        gbc_lblQaOwner.anchor = GridBagConstraints.WEST;
        gbc_lblQaOwner.insets = new Insets(0, 0, 5, 5);
        gbc_lblQaOwner.gridx = 0;
        gbc_lblQaOwner.gridy = 1;
        detailsPanel.add(lblQaOwner, gbc_lblQaOwner);

        txtfldQaowner = new JXTextField();
        txtfldQaowner.setEditable(false);
        txtfldQaowner.setOpaque(false);
        txtfldQaowner.setBackground(new Color(0, 0, 0, 0));
        txtfldQaowner.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldQaowner = new GridBagConstraints();
        gbc_txtfldQaowner.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldQaowner.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldQaowner.gridx = 1;
        gbc_txtfldQaowner.gridy = 1;
        detailsPanel.add(txtfldQaowner, gbc_txtfldQaowner);

        lblOwner = new JXLabel();
        lblOwner.setBorder(new EmptyBorder(0, 10, 0, 10));
        lblOwner.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblOwner.setText("Owner");
        GridBagConstraints gbc_lblOwner = new GridBagConstraints();
        gbc_lblOwner.anchor = GridBagConstraints.WEST;
        gbc_lblOwner.insets = new Insets(0, 0, 5, 5);
        gbc_lblOwner.gridx = 2;
        gbc_lblOwner.gridy = 1;
        detailsPanel.add(lblOwner, gbc_lblOwner);

        txtfldOwner = new JXTextField();
        txtfldOwner.setEditable(false);
        txtfldOwner.setOpaque(false);
        txtfldOwner.setBackground(new Color(0, 0, 0, 0));
        txtfldOwner.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldOwner = new GridBagConstraints();
        gbc_txtfldOwner.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldOwner.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldOwner.gridx = 3;
        gbc_txtfldOwner.gridy = 1;
        detailsPanel.add(txtfldOwner, gbc_txtfldOwner);

        lblAuthor = new JXLabel();
        lblAuthor.setBorder(new EmptyBorder(0, 0, 0, 10));
        lblAuthor.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblAuthor.setText("Author");
        GridBagConstraints gbc_lblAuthor = new GridBagConstraints();
        gbc_lblAuthor.anchor = GridBagConstraints.WEST;
        gbc_lblAuthor.insets = new Insets(0, 0, 5, 5);
        gbc_lblAuthor.gridx = 0;
        gbc_lblAuthor.gridy = 2;
        detailsPanel.add(lblAuthor, gbc_lblAuthor);

        txtfldAuthor = new JXTextField();
        txtfldAuthor.setEditable(false);
        txtfldAuthor.setOpaque(false);
        txtfldAuthor.setBackground(new Color(0, 0, 0, 0));
        txtfldAuthor.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldAuthor = new GridBagConstraints();
        gbc_txtfldAuthor.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldAuthor.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldAuthor.gridx = 1;
        gbc_txtfldAuthor.gridy = 2;
        detailsPanel.add(txtfldAuthor, gbc_txtfldAuthor);

        lblFeature = new JXLabel();
        lblFeature.setBorder(new EmptyBorder(0, 10, 0, 10));
        lblFeature.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblFeature.setText("Feature");
        GridBagConstraints gbc_lblFeature = new GridBagConstraints();
        gbc_lblFeature.insets = new Insets(0, 0, 5, 5);
        gbc_lblFeature.anchor = GridBagConstraints.WEST;
        gbc_lblFeature.gridx = 2;
        gbc_lblFeature.gridy = 2;
        detailsPanel.add(lblFeature, gbc_lblFeature);

        txtfldFeature = new JXTextField();
        txtfldFeature.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldFeature.setEditable(false);
        txtfldFeature.setOpaque(false);
        txtfldFeature.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldFeature = new GridBagConstraints();
        gbc_txtfldFeature.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldFeature.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldFeature.gridx = 3;
        gbc_txtfldFeature.gridy = 2;
        detailsPanel.add(txtfldFeature, gbc_txtfldFeature);

        lblPhase = new JXLabel();
        lblPhase.setBorder(new EmptyBorder(0, 0, 0, 10));
        lblPhase.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblPhase.setText("Phase");
        GridBagConstraints gbc_lblPhase = new GridBagConstraints();
        gbc_lblPhase.anchor = GridBagConstraints.WEST;
        gbc_lblPhase.insets = new Insets(0, 0, 5, 5);
        gbc_lblPhase.gridx = 0;
        gbc_lblPhase.gridy = 3;
        detailsPanel.add(lblPhase, gbc_lblPhase);

        comboBoxPhase = new JComboBox();
        comboBoxPhase.setEditable(true);
        comboBoxPhase.setBorder(new EmptyBorder(0, 0, 0, 0));
        comboBoxPhase.setModel(new DefaultComboBoxModel(new String[]{"Open", "In Progress", "Done"}));
        GridBagConstraints gbc_comboBox = new GridBagConstraints();
        gbc_comboBox.insets = new Insets(0, 0, 5, 5);
        gbc_comboBox.fill = GridBagConstraints.HORIZONTAL;
        gbc_comboBox.gridx = 1;
        gbc_comboBox.gridy = 3;
        detailsPanel.add(comboBoxPhase, gbc_comboBox);

        lblRelease = new JXLabel();
        lblRelease.setText("Release");
        lblRelease.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblRelease.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblRelease = new GridBagConstraints();
        gbc_lblRelease.anchor = GridBagConstraints.WEST;
        gbc_lblRelease.insets = new Insets(0, 0, 5, 5);
        gbc_lblRelease.gridx = 0;
        gbc_lblRelease.gridy = 5;
        detailsPanel.add(lblRelease, gbc_lblRelease);

        txtfldRelease = new JXTextField();
        txtfldRelease.setEditable(false);
        txtfldRelease.setOpaque(false);
        txtfldRelease.setBackground(new Color(0, 0, 0, 0));
        txtfldRelease.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldRelease = new GridBagConstraints();
        gbc_txtfldRelease.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldRelease.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldRelease.gridx = 1;
        gbc_txtfldRelease.gridy = 5;
        detailsPanel.add(txtfldRelease, gbc_txtfldRelease);

        lblSprint = new JXLabel();
        lblSprint.setText("Sprint");
        lblSprint.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblSprint.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblSprint = new GridBagConstraints();
        gbc_lblSprint.anchor = GridBagConstraints.WEST;
        gbc_lblSprint.insets = new Insets(0, 0, 5, 5);
        gbc_lblSprint.gridx = 2;
        gbc_lblSprint.gridy = 5;
        detailsPanel.add(lblSprint, gbc_lblSprint);

        txtfldSprint = new JXTextField();
        txtfldSprint.setEditable(false);
        txtfldSprint.setOpaque(false);
        txtfldSprint.setBackground(new Color(0, 0, 0, 0));
        txtfldSprint.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldSprint = new GridBagConstraints();
        gbc_txtfldSprint.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldSprint.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldSprint.gridx = 3;
        gbc_txtfldSprint.gridy = 5;
        detailsPanel.add(txtfldSprint, gbc_txtfldSprint);

        lblStoryPoints = new JXLabel();
        lblStoryPoints.setText("Story Points");
        lblStoryPoints.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblStoryPoints.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblStoryPoints = new GridBagConstraints();
        gbc_lblStoryPoints.anchor = GridBagConstraints.WEST;
        gbc_lblStoryPoints.insets = new Insets(0, 0, 5, 5);
        gbc_lblStoryPoints.gridx = 0;
        gbc_lblStoryPoints.gridy = 7;
        detailsPanel.add(lblStoryPoints, gbc_lblStoryPoints);

        txtfldStorypoints = new JXTextField();
        txtfldStorypoints.setEditable(false);
        txtfldStorypoints.setOpaque(false);
        txtfldStorypoints.setBackground(new Color(0, 0, 0, 0));
        txtfldStorypoints.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldStorypoints = new GridBagConstraints();
        gbc_txtfldStorypoints.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldStorypoints.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldStorypoints.gridx = 1;
        gbc_txtfldStorypoints.gridy = 7;
        detailsPanel.add(txtfldStorypoints, gbc_txtfldStorypoints);

        lblQaStoryPoints = new JXLabel();
        lblQaStoryPoints.setText("QA Story Points Days");
        lblQaStoryPoints.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblQaStoryPoints.setBorder(new EmptyBorder(0, 10, 0, 10));
        lblQaStoryPoints.setOpaque(false);
        lblQaStoryPoints.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_lblQaStoryPoints = new GridBagConstraints();
        gbc_lblQaStoryPoints.anchor = GridBagConstraints.WEST;
        gbc_lblQaStoryPoints.insets = new Insets(0, 0, 5, 5);
        gbc_lblQaStoryPoints.gridx = 2;
        gbc_lblQaStoryPoints.gridy = 7;
        detailsPanel.add(lblQaStoryPoints, gbc_lblQaStoryPoints);

        txtfldQastorypointsdays = new JXTextField();
        txtfldQastorypointsdays.setEditable(false);
        txtfldQastorypointsdays.setOpaque(false);
        txtfldQastorypointsdays.setBackground(new Color(0, 0, 0, 0));
        txtfldQastorypointsdays.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldQastorypointsdays = new GridBagConstraints();
        gbc_txtfldQastorypointsdays.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldQastorypointsdays.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldQastorypointsdays.gridx = 3;
        gbc_txtfldQastorypointsdays.gridy = 7;
        detailsPanel.add(txtfldQastorypointsdays, gbc_txtfldQastorypointsdays);

        lblDevStoryPoints = new JXLabel();
        lblDevStoryPoints.setText("Dev Story Points Days");
        lblDevStoryPoints.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDevStoryPoints.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblDevStoryPoints = new GridBagConstraints();
        gbc_lblDevStoryPoints.anchor = GridBagConstraints.EAST;
        gbc_lblDevStoryPoints.insets = new Insets(0, 0, 0, 5);
        gbc_lblDevStoryPoints.gridx = 0;
        gbc_lblDevStoryPoints.gridy = 8;
        detailsPanel.add(lblDevStoryPoints, gbc_lblDevStoryPoints);

        txtfldDevstorypointsdays = new JXTextField();
        txtfldDevstorypointsdays.setEditable(false);
        txtfldDevstorypointsdays.setOpaque(false);
        txtfldDevstorypointsdays.setBackground(new Color(0, 0, 0, 0));
        txtfldDevstorypointsdays.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldDevstorypointsdays = new GridBagConstraints();
        gbc_txtfldDevstorypointsdays.insets = new Insets(0, 0, 0, 5);
        gbc_txtfldDevstorypointsdays.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldDevstorypointsdays.gridx = 1;
        gbc_txtfldDevstorypointsdays.gridy = 8;
        detailsPanel.add(txtfldDevstorypointsdays, gbc_txtfldDevstorypointsdays);
    }

    public void setTxtfldDescription(String txtfldDescription) {
        this.txtfldDescription.setText(txtfldDescription);
    }

    public void setTxtfldFeature(String txtfldFeature) {
        this.txtfldFeature.setText(txtfldFeature);
    }

    public void setTxtfldTeam(String txtfldTeam) {
        this.txtfldTeam.setText(txtfldTeam);
    }

    public void setTxtfldGroup(String txtfldGroup) {
        this.txtfldGroup.setText(txtfldGroup);
    }

    public void setTxtfldQaowner(String txtfldQaowner) {
        this.txtfldQaowner.setText(txtfldQaowner);
    }

    public void setTxtfldOwner(String txtfldOwner) {
        this.txtfldOwner.setText(txtfldOwner);
    }

    public void setTxtfldRelease(String txtfldRelease) {
        this.txtfldRelease.setText(txtfldRelease);
    }

    public void setTxtfldSprint(String txtfldSprint) {
        this.txtfldSprint.setText(txtfldSprint);
    }

    public void setTxtfldAuthor(String txtfldAuthor) {
        this.txtfldAuthor.setText(txtfldAuthor);
    }

    public void setTxtfldStoryPoints(String txtfldStorypoints) {
        this.txtfldStorypoints.setText(txtfldStorypoints);
    }

    public void setTxtfldQastorypointsdays(String txtfldQastorypointsdays) {
        this.txtfldQastorypointsdays.setText(txtfldQastorypointsdays);
    }

    public void setTxtfldDevstorypointsdays(String txtfldDevstorypointsdays) {
        this.txtfldDevstorypointsdays.setText(txtfldDevstorypointsdays);
    }

    public String getComboBoxPhase() {
        return comboBoxPhase.getSelectedItem().toString();
    }

    public void setComboBoxPhase(String phaseName) {
         this.comboBoxPhase.getEditor().setItem(phaseName);
    }

    public void setLblName(String lblName) {
        this.lblName.setText("Name: " + lblName);
    }
}
