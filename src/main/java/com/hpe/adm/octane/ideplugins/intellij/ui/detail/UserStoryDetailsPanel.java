package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextArea;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;

public class UserStoryDetailsPanel extends JPanel {
    private JXPanel rootPanel;
    private JXLabel lblDescription;
    private JXTextArea txtfldDescription;
    private JXPanel userStoryDetailsPanel;
    private JXLabel lblTeam;
    private JXTextField txtfldTeam;
    private JXLabel lblGroup;
    private JXTextField txtfldGroup;
    private JXLabel lblQaOwner;
    private JXTextField txtfldQaowner;
    private JXLabel lblOwner;
    private JXTextField txtfldOwner;
    private JXLabel lblAuthor;
    private JXTextField txtfldAuthor;
    private JXLabel lblFeature;
    private JXTextField txtfldFeature;
    private JXLabel lblPhase;
    private JComboBox comboBoxPhase;
    private JXLabel lblRelease;
    private JXTextField txtfldRelease;
    private JXLabel lblSprint;
    private JXTextField txtfldSprint;
    private JXLabel lblStoryPoints;
    private JXTextField txtfldStoryPoints;
    private JXLabel lblQaStoryPointsDays;
    private JXTextField txtfldQaStoryPointsDays;
    private JXLabel lblDevStoryPointsDays;
    private JXTextField txtfldDevStoryPointsDays;
    private JXPanel nameAndIconPanel;
    private JXLabel lblName;

    public UserStoryDetailsPanel() {
        setBorder(null);
        setBounds(100, 100, 900, 336);
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0};
        gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{1.0, Double.MIN_VALUE};
        this.setLayout(gridBagLayout);

        rootPanel = new JXPanel();
        rootPanel.setBorder(null);
        GridBagConstraints gbc_rootPanel = new GridBagConstraints();
        gbc_rootPanel.fill = GridBagConstraints.BOTH;
        gbc_rootPanel.gridx = 0;
        gbc_rootPanel.gridy = 0;
        add(rootPanel, gbc_rootPanel);
        GridBagLayout gbl_rootPanel = new GridBagLayout();
        gbl_rootPanel.columnWidths = new int[]{0, 0};
        gbl_rootPanel.rowHeights = new int[]{0, 0, 0, 0, 0};
        gbl_rootPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_rootPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
        rootPanel.setLayout(gbl_rootPanel);

        nameAndIconPanel = new JXPanel();
        FlowLayout flowLayout = (FlowLayout) nameAndIconPanel.getLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        nameAndIconPanel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_nameAndIconPanel = new GridBagConstraints();
        gbc_nameAndIconPanel.insets = new Insets(0, 0, 5, 0);
        gbc_nameAndIconPanel.fill = GridBagConstraints.BOTH;
        gbc_nameAndIconPanel.gridx = 0;
        gbc_nameAndIconPanel.gridy = 0;
        rootPanel.add(nameAndIconPanel, gbc_nameAndIconPanel);
        
        lblName = new JXLabel();
        lblName.setIcon(new ImageIcon(UserStoryDetailsPanel.class.getResource("/images/userStoryIcon.png")));
        lblName.setText("Name");
        lblName.setFont(new Font("Tahoma", Font.BOLD, 12));
        lblName.setBorder(null);
        nameAndIconPanel.add(lblName);
        
        lblDescription = new JXLabel();
        lblDescription.setText("Description");
        lblDescription.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDescription.setBorder(new EmptyBorder(5, 10, 0, 10));
        GridBagConstraints gbc_lblDescription = new GridBagConstraints();
        gbc_lblDescription.anchor = GridBagConstraints.NORTHWEST;
        gbc_lblDescription.insets = new Insets(0, 0, 5, 0);
        gbc_lblDescription.gridx = 0;
        gbc_lblDescription.gridy = 1;
        rootPanel.add(lblDescription, gbc_lblDescription);
        
        txtfldDescription = new JXTextArea();
        txtfldDescription.setText("Description");
        txtfldDescription.setOpaque(false);
        txtfldDescription.setLineWrap(true);
        txtfldDescription.setEditable(false);
        txtfldDescription.setBorder(new EmptyBorder(5, 10, 0, 10));
        txtfldDescription.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldDescription = new GridBagConstraints();
        gbc_txtfldDescription.fill = GridBagConstraints.BOTH;
        gbc_txtfldDescription.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldDescription.gridx = 0;
        gbc_txtfldDescription.gridy = 2;
        rootPanel.add(txtfldDescription, gbc_txtfldDescription);

        userStoryDetailsPanel = new JXPanel();
        userStoryDetailsPanel.setBorder(new EmptyBorder(10, 10, 2, 10));
        GridBagConstraints gbc_userStoryDetailsPanel = new GridBagConstraints();
        gbc_userStoryDetailsPanel.fill = GridBagConstraints.BOTH;
        gbc_userStoryDetailsPanel.gridx = 0;
        gbc_userStoryDetailsPanel.gridy = 3;
        rootPanel.add(userStoryDetailsPanel, gbc_userStoryDetailsPanel);
        GridBagLayout gbl_userStoryDetailsPanel = new GridBagLayout();
        gbl_userStoryDetailsPanel.columnWidths = new int[]{0, 0, 0, 0, 0};
        gbl_userStoryDetailsPanel.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_userStoryDetailsPanel.columnWeights = new double[]{0.0, 1.0, 0.0, 1.0, Double.MIN_VALUE};
        gbl_userStoryDetailsPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        userStoryDetailsPanel.setLayout(gbl_userStoryDetailsPanel);
        
        lblTeam = new JXLabel();
        lblTeam.setText("Team");
        lblTeam.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblTeam.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblTeam = new GridBagConstraints();
        gbc_lblTeam.anchor = GridBagConstraints.WEST;
        gbc_lblTeam.insets = new Insets(0, 0, 5, 5);
        gbc_lblTeam.gridx = 0;
        gbc_lblTeam.gridy = 0;
        userStoryDetailsPanel.add(lblTeam, gbc_lblTeam);
        
        txtfldTeam = new JXTextField();
        txtfldTeam.setOpaque(false);
        txtfldTeam.setEditable(false);
        txtfldTeam.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldTeam.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldTeam = new GridBagConstraints();
        gbc_txtfldTeam.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldTeam.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldTeam.gridx = 1;
        gbc_txtfldTeam.gridy = 0;
        userStoryDetailsPanel.add(txtfldTeam, gbc_txtfldTeam);
        
        lblGroup = new JXLabel();
        lblGroup.setText("Group");
        lblGroup.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblGroup.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblGroup = new GridBagConstraints();
        gbc_lblGroup.anchor = GridBagConstraints.WEST;
        gbc_lblGroup.insets = new Insets(0, 0, 5, 5);
        gbc_lblGroup.gridx = 2;
        gbc_lblGroup.gridy = 0;
        userStoryDetailsPanel.add(lblGroup, gbc_lblGroup);
        
        txtfldGroup = new JXTextField();
        txtfldGroup.setOpaque(false);
        txtfldGroup.setEditable(false);
        txtfldGroup.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldGroup.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldGroup = new GridBagConstraints();
        gbc_txtfldGroup.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldGroup.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldGroup.gridx = 3;
        gbc_txtfldGroup.gridy = 0;
        userStoryDetailsPanel.add(txtfldGroup, gbc_txtfldGroup);
        
        lblQaOwner = new JXLabel();
        lblQaOwner.setText("QA Owner");
        lblQaOwner.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblQaOwner.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblQaOwner = new GridBagConstraints();
        gbc_lblQaOwner.anchor = GridBagConstraints.WEST;
        gbc_lblQaOwner.insets = new Insets(0, 0, 5, 5);
        gbc_lblQaOwner.gridx = 0;
        gbc_lblQaOwner.gridy = 1;
        userStoryDetailsPanel.add(lblQaOwner, gbc_lblQaOwner);
        
        txtfldQaowner = new JXTextField();
        txtfldQaowner.setOpaque(false);
        txtfldQaowner.setEditable(false);
        txtfldQaowner.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldQaowner.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldQaowner = new GridBagConstraints();
        gbc_txtfldQaowner.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldQaowner.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldQaowner.gridx = 1;
        gbc_txtfldQaowner.gridy = 1;
        userStoryDetailsPanel.add(txtfldQaowner, gbc_txtfldQaowner);
        
        lblOwner = new JXLabel();
        lblOwner.setText("Owner");
        lblOwner.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblOwner.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblOwner = new GridBagConstraints();
        gbc_lblOwner.anchor = GridBagConstraints.WEST;
        gbc_lblOwner.insets = new Insets(0, 0, 5, 5);
        gbc_lblOwner.gridx = 2;
        gbc_lblOwner.gridy = 1;
        userStoryDetailsPanel.add(lblOwner, gbc_lblOwner);
        
        txtfldOwner = new JXTextField();
        txtfldOwner.setOpaque(false);
        txtfldOwner.setEditable(false);
        txtfldOwner.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldOwner.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldOwner = new GridBagConstraints();
        gbc_txtfldOwner.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldOwner.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldOwner.gridx = 3;
        gbc_txtfldOwner.gridy = 1;
        userStoryDetailsPanel.add(txtfldOwner, gbc_txtfldOwner);
        
        lblAuthor = new JXLabel();
        lblAuthor.setText("Author");
        lblAuthor.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblAuthor.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblAuthor = new GridBagConstraints();
        gbc_lblAuthor.anchor = GridBagConstraints.WEST;
        gbc_lblAuthor.insets = new Insets(0, 0, 5, 5);
        gbc_lblAuthor.gridx = 0;
        gbc_lblAuthor.gridy = 2;
        userStoryDetailsPanel.add(lblAuthor, gbc_lblAuthor);
        
        txtfldAuthor = new JXTextField();
        txtfldAuthor.setOpaque(false);
        txtfldAuthor.setEditable(false);
        txtfldAuthor.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldAuthor.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldAuthor = new GridBagConstraints();
        gbc_txtfldAuthor.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldAuthor.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldAuthor.gridx = 1;
        gbc_txtfldAuthor.gridy = 2;
        userStoryDetailsPanel.add(txtfldAuthor, gbc_txtfldAuthor);
        
        lblFeature = new JXLabel();
        lblFeature.setText("Feature");
        lblFeature.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblFeature.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblFeature = new GridBagConstraints();
        gbc_lblFeature.anchor = GridBagConstraints.WEST;
        gbc_lblFeature.insets = new Insets(0, 0, 5, 5);
        gbc_lblFeature.gridx = 2;
        gbc_lblFeature.gridy = 2;
        userStoryDetailsPanel.add(lblFeature, gbc_lblFeature);
        
        txtfldFeature = new JXTextField();
        txtfldFeature.setOpaque(false);
        txtfldFeature.setEditable(false);
        txtfldFeature.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldFeature.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldFeature = new GridBagConstraints();
        gbc_txtfldFeature.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldFeature.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldFeature.gridx = 3;
        gbc_txtfldFeature.gridy = 2;
        userStoryDetailsPanel.add(txtfldFeature, gbc_txtfldFeature);
        
        lblPhase = new JXLabel();
        lblPhase.setText("Phase");
        lblPhase.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblPhase.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblPhase = new GridBagConstraints();
        gbc_lblPhase.anchor = GridBagConstraints.WEST;
        gbc_lblPhase.insets = new Insets(0, 0, 5, 5);
        gbc_lblPhase.gridx = 0;
        gbc_lblPhase.gridy = 3;
        userStoryDetailsPanel.add(lblPhase, gbc_lblPhase);
        
        comboBoxPhase = new JComboBox();
        comboBoxPhase.setEditable(true);
        comboBoxPhase.setBorder(new EmptyBorder(0, 0, 0, 0));
        GridBagConstraints gbc_comboBoxPhase = new GridBagConstraints();
        gbc_comboBoxPhase.fill = GridBagConstraints.HORIZONTAL;
        gbc_comboBoxPhase.insets = new Insets(0, 0, 5, 5);
        gbc_comboBoxPhase.gridx = 1;
        gbc_comboBoxPhase.gridy = 3;
        userStoryDetailsPanel.add(comboBoxPhase, gbc_comboBoxPhase);
        
        lblRelease = new JXLabel();
        lblRelease.setText("Release");
        lblRelease.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblRelease.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblRelease = new GridBagConstraints();
        gbc_lblRelease.anchor = GridBagConstraints.WEST;
        gbc_lblRelease.insets = new Insets(0, 0, 5, 5);
        gbc_lblRelease.gridx = 0;
        gbc_lblRelease.gridy = 5;
        userStoryDetailsPanel.add(lblRelease, gbc_lblRelease);
        
        txtfldRelease = new JXTextField();
        txtfldRelease.setOpaque(false);
        txtfldRelease.setEditable(false);
        txtfldRelease.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldRelease.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldRelease = new GridBagConstraints();
        gbc_txtfldRelease.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldRelease.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldRelease.gridx = 1;
        gbc_txtfldRelease.gridy = 5;
        userStoryDetailsPanel.add(txtfldRelease, gbc_txtfldRelease);
        
        lblSprint = new JXLabel();
        lblSprint.setText("Sprint");
        lblSprint.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblSprint.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblSprint = new GridBagConstraints();
        gbc_lblSprint.anchor = GridBagConstraints.WEST;
        gbc_lblSprint.insets = new Insets(0, 0, 5, 5);
        gbc_lblSprint.gridx = 2;
        gbc_lblSprint.gridy = 5;
        userStoryDetailsPanel.add(lblSprint, gbc_lblSprint);
        
        txtfldSprint = new JXTextField();
        txtfldSprint.setOpaque(false);
        txtfldSprint.setEditable(false);
        txtfldSprint.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldSprint.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldSprint = new GridBagConstraints();
        gbc_txtfldSprint.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldSprint.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldSprint.gridx = 3;
        gbc_txtfldSprint.gridy = 5;
        userStoryDetailsPanel.add(txtfldSprint, gbc_txtfldSprint);
        
        lblStoryPoints = new JXLabel();
        lblStoryPoints.setText("Story Points");
        lblStoryPoints.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblStoryPoints.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblStoryPoints = new GridBagConstraints();
        gbc_lblStoryPoints.anchor = GridBagConstraints.WEST;
        gbc_lblStoryPoints.insets = new Insets(0, 0, 5, 5);
        gbc_lblStoryPoints.gridx = 0;
        gbc_lblStoryPoints.gridy = 7;
        userStoryDetailsPanel.add(lblStoryPoints, gbc_lblStoryPoints);

        txtfldStoryPoints = new JXTextField();
        txtfldStoryPoints.setOpaque(false);
        txtfldStoryPoints.setEditable(false);
        txtfldStoryPoints.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldStoryPoints.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldStoryPoints = new GridBagConstraints();
        gbc_txtfldStoryPoints.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldStoryPoints.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldStoryPoints.gridx = 1;
        gbc_txtfldStoryPoints.gridy = 7;
        userStoryDetailsPanel.add(txtfldStoryPoints, gbc_txtfldStoryPoints);

        lblQaStoryPointsDays = new JXLabel();
        lblQaStoryPointsDays.setText("QA Story Points Days");
        lblQaStoryPointsDays.setOpaque(false);
        lblQaStoryPointsDays.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblQaStoryPointsDays.setBorder(new EmptyBorder(0, 10, 0, 10));
        lblQaStoryPointsDays.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_lblQaStoryPointsDays = new GridBagConstraints();
        gbc_lblQaStoryPointsDays.anchor = GridBagConstraints.WEST;
        gbc_lblQaStoryPointsDays.insets = new Insets(0, 0, 5, 5);
        gbc_lblQaStoryPointsDays.gridx = 2;
        gbc_lblQaStoryPointsDays.gridy = 7;
        userStoryDetailsPanel.add(lblQaStoryPointsDays, gbc_lblQaStoryPointsDays);

        txtfldQaStoryPointsDays = new JXTextField();
        txtfldQaStoryPointsDays.setOpaque(false);
        txtfldQaStoryPointsDays.setEditable(false);
        txtfldQaStoryPointsDays.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldQaStoryPointsDays.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldQaStoryPointsDays = new GridBagConstraints();
        gbc_txtfldQaStoryPointsDays.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldQaStoryPointsDays.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldQaStoryPointsDays.gridx = 3;
        gbc_txtfldQaStoryPointsDays.gridy = 7;
        userStoryDetailsPanel.add(txtfldQaStoryPointsDays, gbc_txtfldQaStoryPointsDays);

        lblDevStoryPointsDays = new JXLabel();
        lblDevStoryPointsDays.setText("Dev Story Points Days");
        lblDevStoryPointsDays.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDevStoryPointsDays.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblDevStoryPointsDays = new GridBagConstraints();
        gbc_lblDevStoryPointsDays.anchor = GridBagConstraints.EAST;
        gbc_lblDevStoryPointsDays.insets = new Insets(0, 0, 0, 5);
        gbc_lblDevStoryPointsDays.gridx = 0;
        gbc_lblDevStoryPointsDays.gridy = 8;
        userStoryDetailsPanel.add(lblDevStoryPointsDays, gbc_lblDevStoryPointsDays);

        txtfldDevStoryPointsDays = new JXTextField();
        txtfldDevStoryPointsDays.setOpaque(false);
        txtfldDevStoryPointsDays.setEditable(false);
        txtfldDevStoryPointsDays.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldDevStoryPointsDays.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldDevStoryPointsDays = new GridBagConstraints();
        gbc_txtfldDevStoryPointsDays.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldDevStoryPointsDays.insets = new Insets(0, 0, 0, 5);
        gbc_txtfldDevStoryPointsDays.gridx = 1;
        gbc_txtfldDevStoryPointsDays.gridy = 8;
        userStoryDetailsPanel.add(txtfldDevStoryPointsDays, gbc_txtfldDevStoryPointsDays);
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
        this.txtfldStoryPoints.setText(txtfldStorypoints);
    }

    public void setTxtfldQastorypointsdays(String txtfldQastorypointsdays) {
        this.txtfldQaStoryPointsDays.setText(txtfldQastorypointsdays);
    }

    public void setTxtfldDevstorypointsdays(String txtfldDevstorypointsdays) {
        this.txtfldDevStoryPointsDays.setText(txtfldDevstorypointsdays);
    }

    public String getComboBoxPhase() {
        return comboBoxPhase.getSelectedItem().toString();
    }

    public void setComboBoxPhase(String phaseName) {
         this.comboBoxPhase.getEditor().setItem(phaseName);
    }

    public void setLblName(String lblName) {
        this.lblName.setText(lblName);
    }
}
