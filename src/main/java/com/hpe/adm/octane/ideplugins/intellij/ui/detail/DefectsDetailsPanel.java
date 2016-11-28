package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextArea;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.util.HashMap;

public class DefectsDetailsPanel extends JPanel {

    private JXPanel detailsPanel;

    private JXLabel lblDescription;
    private JXTextArea txtfldDescription;

    private JXLabel lblFeature;
    private JXTextField txtfldFeature;

    private JXLabel lblQaOwner;
    private JXTextField txtfldQaOwner;

    private JXLabel lblTeam;
    private JXTextField txtfldTeam;

    private JXLabel lblPriority;
    private JXTextField textfldPriority;

    private JXLabel lblPhase;
    private JComboBox comboBoxPhase;

    private JXLabel lblRelease;
    private JXTextField txtfldRelease;

    private JXLabel lblFixedInPush;
    private JXTextField textfldFixedInPush;

    private JXLabel lblSprint;
    private JXTextField txtfldSprint;

    private JXLabel lblDetectedBy;
    private JXTextField txtfldDetectedBy;

    private JXLabel lblSeverity;
    private JXTextField txtfldSeverity;

    private JXLabel lblGroup;
    private JXTextField txtfldGroup;

    private JXLabel lblEnviroment;
    private JXTextField txtfldEnviroment;

    private JXLabel lblFeedbackType;
    private JXTextField txtfldFeedbackType;

    private JXLabel lblStoryPoints;
    private JXTextField textfldStoryPoints;

    private JXLabel lblDetectedInPush;
    private JXTextField textfldDetectedInPush;

    private JXLabel lblDetectedInRelease;
    private JXTextField txtfldDetectedInRelease;
    private HashMap<String, Integer> phases = new HashMap<>();
    private JXTextField invizibleSpacerL;
    private JXTextField invizibleSpacerR;
    private JXLabel lblBlocked;
    private JXTextField txtfldBlocked;
    private JXLabel lblBlockedReason;
    private JXLabel lblAppModule;
    private JXPanel panel;
    private JXLabel lblAttachments;
    private JXLabel lblLinksToFile;
    private JXTextField txtfldBlockedReason;
    private JXTextField txtfldAppModules;

    public DefectsDetailsPanel() {
        phases.put("Duplicate", 6);
        phases.put("Closed", 5);
        phases.put("Ready for QA", 4);
        phases.put("In Testing", 3);
        phases.put("In Progress", 2);
        phases.put("Open", 1);

        setBounds(100, 100, 814, 348);
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
        parentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        scrollPane.setViewportView(parentPanel);
        GridBagLayout gbl_parentPanel = new GridBagLayout();
        gbl_parentPanel.columnWidths = new int[]{0, 0};
        gbl_parentPanel.rowHeights = new int[]{0, 0, 0, 0, 0};
        gbl_parentPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_parentPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
        parentPanel.setLayout(gbl_parentPanel);

        lblDescription = new JXLabel();
        lblDescription.setBorder(new EmptyBorder(7, 7, 0, 0));
        lblDescription.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDescription.setText("Description");
        GridBagConstraints gbc_lblDescription = new GridBagConstraints();
        gbc_lblDescription.anchor = GridBagConstraints.NORTHWEST;
        gbc_lblDescription.insets = new Insets(0, 0, 5, 0);
        gbc_lblDescription.gridx = 0;
        gbc_lblDescription.gridy = 0;
        parentPanel.add(lblDescription, gbc_lblDescription);

        txtfldDescription = new JXTextArea();
        txtfldDescription.setOpaque(false);
        txtfldDescription.setBackground(new Color(0, 0, 0, 0));
        txtfldDescription.setBorder(new EmptyBorder(5, 7, 0, 13));
        txtfldDescription.setLineWrap(true);
        GridBagConstraints gbc_txtrDescription = new GridBagConstraints();
        gbc_txtrDescription.insets = new Insets(0, 0, 5, 0);
        gbc_txtrDescription.fill = GridBagConstraints.BOTH;
        gbc_txtrDescription.gridx = 0;
        gbc_txtrDescription.gridy = 1;
        parentPanel.add(txtfldDescription, gbc_txtrDescription);

        detailsPanel = new JXPanel();
        detailsPanel.setOpaque(false);
        detailsPanel.setBorder(new EmptyBorder(10, 7, 2, 13));
        GridBagConstraints gbc_detailsPanel = new GridBagConstraints();
        gbc_detailsPanel.insets = new Insets(0, 0, 5, 0);
        gbc_detailsPanel.fill = GridBagConstraints.BOTH;
        gbc_detailsPanel.gridx = 0;
        gbc_detailsPanel.gridy = 2;
        parentPanel.add(detailsPanel, gbc_detailsPanel);
        GridBagLayout gbl_detailsPanel = new GridBagLayout();
        gbl_detailsPanel.columnWidths = new int[]{0, 0, 0, 0, 0};
        gbl_detailsPanel.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanel.columnWeights = new double[]{0.0, 1.0, 0.0, 1.0, Double.MIN_VALUE};
        gbl_detailsPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanel.setLayout(gbl_detailsPanel);

        lblFeature = new JXLabel();
        lblFeature.setBorder(new EmptyBorder(0, 0, 0, 10));
        lblFeature.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblFeature.setText("Feature");
        GridBagConstraints gbc_lblFeature = new GridBagConstraints();
        gbc_lblFeature.insets = new Insets(0, 0, 5, 5);
        gbc_lblFeature.anchor = GridBagConstraints.WEST;
        gbc_lblFeature.gridx = 0;
        gbc_lblFeature.gridy = 0;
        detailsPanel.add(lblFeature, gbc_lblFeature);

        txtfldFeature = new JXTextField();
        txtfldFeature.setOpaque(false);
        txtfldFeature.setBackground(new Color(0, 0, 0, 0));
        txtfldFeature.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldFeature.setEditable(false);
        GridBagConstraints gbc_txtfldFeature = new GridBagConstraints();
        gbc_txtfldFeature.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldFeature.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldFeature.gridx = 1;
        gbc_txtfldFeature.gridy = 0;
        detailsPanel.add(txtfldFeature, gbc_txtfldFeature);

        lblDetectedBy = new JXLabel();
        lblDetectedBy.setBorder(new EmptyBorder(0, 20, 0, 10));
        lblDetectedBy.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDetectedBy.setText("Detected by");
        GridBagConstraints gbc_lblDetectedBy = new GridBagConstraints();
        gbc_lblDetectedBy.insets = new Insets(0, 0, 5, 5);
        gbc_lblDetectedBy.anchor = GridBagConstraints.SOUTHWEST;
        gbc_lblDetectedBy.gridx = 2;
        gbc_lblDetectedBy.gridy = 0;
        detailsPanel.add(lblDetectedBy, gbc_lblDetectedBy);

        txtfldDetectedBy = new JXTextField();
        txtfldDetectedBy.setOpaque(false);
        txtfldDetectedBy.setBackground(new Color(0, 0, 0, 0));
        txtfldDetectedBy.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldDetectedBy.setEditable(false);
        GridBagConstraints gbc_txtfldDetectedBy = new GridBagConstraints();
        gbc_txtfldDetectedBy.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldDetectedBy.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldDetectedBy.gridx = 3;
        gbc_txtfldDetectedBy.gridy = 0;
        detailsPanel.add(txtfldDetectedBy, gbc_txtfldDetectedBy);

        lblQaOwner = new JXLabel();
        lblQaOwner.setBorder(new EmptyBorder(0, 0, 0, 10));
        lblQaOwner.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblQaOwner.setText("QA Owner");
        GridBagConstraints gbc_lblQaOwner = new GridBagConstraints();
        gbc_lblQaOwner.anchor = GridBagConstraints.WEST;
        gbc_lblQaOwner.insets = new Insets(0, 0, 5, 5);
        gbc_lblQaOwner.gridx = 0;
        gbc_lblQaOwner.gridy = 1;
        detailsPanel.add(lblQaOwner, gbc_lblQaOwner);

        txtfldQaOwner = new JXTextField();
        txtfldQaOwner.setOpaque(false);
        txtfldQaOwner.setBackground(new Color(0, 0, 0, 0));
        txtfldQaOwner.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldQaOwner.setEditable(false);
        GridBagConstraints gbc_txtfldQaOwner = new GridBagConstraints();
        gbc_txtfldQaOwner.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldQaOwner.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldQaOwner.gridx = 1;
        gbc_txtfldQaOwner.gridy = 1;
        detailsPanel.add(txtfldQaOwner, gbc_txtfldQaOwner);
                
                lblBlocked = new JXLabel();
                lblBlocked.setText("Blocked");
                lblBlocked.setFont(new Font("Tahoma", Font.BOLD, 11));
                lblBlocked.setBorder(new EmptyBorder(0, 20, 0, 10));
                GridBagConstraints gbc_lblBlocked = new GridBagConstraints();
                gbc_lblBlocked.anchor = GridBagConstraints.SOUTHWEST;
                gbc_lblBlocked.insets = new Insets(0, 0, 5, 5);
                gbc_lblBlocked.gridx = 2;
                gbc_lblBlocked.gridy = 1;
                detailsPanel.add(lblBlocked, gbc_lblBlocked);
                
                txtfldBlocked = new JXTextField();
                txtfldBlocked.setOpaque(false);
                txtfldBlocked.setEditable(false);
                txtfldBlocked.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
                txtfldBlocked.setBackground(new Color(0, 0, 0, 0));
                GridBagConstraints gbc_txtfldBlocked = new GridBagConstraints();
                gbc_txtfldBlocked.insets = new Insets(0, 0, 5, 0);
                gbc_txtfldBlocked.fill = GridBagConstraints.HORIZONTAL;
                gbc_txtfldBlocked.gridx = 3;
                gbc_txtfldBlocked.gridy = 1;
                detailsPanel.add(txtfldBlocked, gbc_txtfldBlocked);
        
                lblSeverity = new JXLabel();
                lblSeverity.setBorder(new EmptyBorder(0, 0, 0, 10));
                lblSeverity.setFont(new Font("Tahoma", Font.BOLD, 11));
                lblSeverity.setText("Severity");
                GridBagConstraints gbc_lblSeverity = new GridBagConstraints();
                gbc_lblSeverity.anchor = GridBagConstraints.SOUTHWEST;
                gbc_lblSeverity.insets = new Insets(0, 0, 5, 5);
                gbc_lblSeverity.gridx = 0;
                gbc_lblSeverity.gridy = 2;
                detailsPanel.add(lblSeverity, gbc_lblSeverity);
        
                txtfldSeverity = new JXTextField();
                txtfldSeverity.setOpaque(false);
                txtfldSeverity.setBackground(new Color(0, 0, 0, 0));
                txtfldSeverity.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
                txtfldSeverity.setEditable(false);
                GridBagConstraints gbc_txtfldSeverity = new GridBagConstraints();
                gbc_txtfldSeverity.insets = new Insets(0, 0, 5, 5);
                gbc_txtfldSeverity.fill = GridBagConstraints.HORIZONTAL;
                gbc_txtfldSeverity.gridx = 1;
                gbc_txtfldSeverity.gridy = 2;
                detailsPanel.add(txtfldSeverity, gbc_txtfldSeverity);
        
        lblBlockedReason = new JXLabel();
        lblBlockedReason.setText("Blocked Reason");
        lblBlockedReason.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblBlockedReason.setBorder(new EmptyBorder(0, 20, 0, 10));
        GridBagConstraints gbc_lblBlockedReason = new GridBagConstraints();
        gbc_lblBlockedReason.anchor = GridBagConstraints.SOUTHWEST;
        gbc_lblBlockedReason.insets = new Insets(0, 0, 5, 5);
        gbc_lblBlockedReason.gridx = 2;
        gbc_lblBlockedReason.gridy = 2;
        detailsPanel.add(lblBlockedReason, gbc_lblBlockedReason);
        
        txtfldBlockedReason = new JXTextField();
        txtfldBlockedReason.setOpaque(false);
        txtfldBlockedReason.setEditable(false);
        txtfldBlockedReason.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldBlockedReason.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldBlockedReason = new GridBagConstraints();
        gbc_txtfldBlockedReason.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldBlockedReason.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldBlockedReason.gridx = 3;
        gbc_txtfldBlockedReason.gridy = 2;
        detailsPanel.add(txtfldBlockedReason, gbc_txtfldBlockedReason);

        lblTeam = new JXLabel();
        lblTeam.setBorder(new EmptyBorder(0, 0, 0, 10));
        lblTeam.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblTeam.setText("Team");
        GridBagConstraints gbc_lblTeam = new GridBagConstraints();
        gbc_lblTeam.anchor = GridBagConstraints.WEST;
        gbc_lblTeam.insets = new Insets(0, 0, 5, 5);
        gbc_lblTeam.gridx = 0;
        gbc_lblTeam.gridy = 3;
        detailsPanel.add(lblTeam, gbc_lblTeam);

        txtfldTeam = new JXTextField();
        txtfldTeam.setOpaque(false);
        txtfldTeam.setBackground(new Color(0, 0, 0, 0));
        txtfldTeam.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldTeam.setEditable(false);
        GridBagConstraints gbc_txtfldTeam = new GridBagConstraints();
        gbc_txtfldTeam.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldTeam.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldTeam.gridx = 1;
        gbc_txtfldTeam.gridy = 3;
        detailsPanel.add(txtfldTeam, gbc_txtfldTeam);
                
                        lblGroup = new JXLabel();
                        lblGroup.setBorder(new EmptyBorder(0, 20, 0, 0));
                        lblGroup.setFont(new Font("Tahoma", Font.BOLD, 11));
                        lblGroup.setText("Group");
                        GridBagConstraints gbc_lblGroup = new GridBagConstraints();
                        gbc_lblGroup.anchor = GridBagConstraints.WEST;
                        gbc_lblGroup.insets = new Insets(0, 0, 5, 5);
                        gbc_lblGroup.gridx = 2;
                        gbc_lblGroup.gridy = 3;
                        detailsPanel.add(lblGroup, gbc_lblGroup);
                
                        txtfldGroup = new JXTextField();
                        txtfldGroup.setOpaque(false);
                        txtfldGroup.setBackground(new Color(0, 0, 0, 0));
                        txtfldGroup.setEditable(false);
                        txtfldGroup.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
                        GridBagConstraints gbc_txtfldGroup = new GridBagConstraints();
                        gbc_txtfldGroup.insets = new Insets(0, 0, 5, 0);
                        gbc_txtfldGroup.fill = GridBagConstraints.HORIZONTAL;
                        gbc_txtfldGroup.gridx = 3;
                        gbc_txtfldGroup.gridy = 3;
                        detailsPanel.add(txtfldGroup, gbc_txtfldGroup);
        
                lblStoryPoints = new JXLabel();
                lblStoryPoints.setBorder(new EmptyBorder(0, 0, 0, 10));
                lblStoryPoints.setFont(new Font("Tahoma", Font.BOLD, 11));
                lblStoryPoints.setText("Story Points");
                GridBagConstraints gbc_lblStoryPoints = new GridBagConstraints();
                gbc_lblStoryPoints.anchor = GridBagConstraints.WEST;
                gbc_lblStoryPoints.insets = new Insets(0, 0, 5, 5);
                gbc_lblStoryPoints.gridx = 0;
                gbc_lblStoryPoints.gridy = 4;
                detailsPanel.add(lblStoryPoints, gbc_lblStoryPoints);
        
                textfldStoryPoints = new JXTextField();
                textfldStoryPoints.setOpaque(false);
                textfldStoryPoints.setBackground(new Color(0, 0, 0, 0));
                textfldStoryPoints.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
                textfldStoryPoints.setEditable(false);
                GridBagConstraints gbc_textField_1 = new GridBagConstraints();
                gbc_textField_1.insets = new Insets(0, 0, 5, 5);
                gbc_textField_1.fill = GridBagConstraints.HORIZONTAL;
                gbc_textField_1.gridx = 1;
                gbc_textField_1.gridy = 4;
                detailsPanel.add(textfldStoryPoints, gbc_textField_1);

        lblFeedbackType = new JXLabel();
        lblFeedbackType.setBorder(new EmptyBorder(0, 20, 0, 10));
        lblFeedbackType.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblFeedbackType.setText("Feedback type");
        GridBagConstraints gbc_lblFeedbackType = new GridBagConstraints();
        gbc_lblFeedbackType.anchor = GridBagConstraints.SOUTHWEST;
        gbc_lblFeedbackType.insets = new Insets(0, 0, 5, 5);
        gbc_lblFeedbackType.gridx = 2;
        gbc_lblFeedbackType.gridy = 4;
        detailsPanel.add(lblFeedbackType, gbc_lblFeedbackType);

        txtfldFeedbackType = new JXTextField();
        txtfldFeedbackType.setOpaque(false);
        txtfldFeedbackType.setBackground(new Color(0, 0, 0, 0));
        txtfldFeedbackType.setEditable(false);
        txtfldFeedbackType.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldFeedbackType = new GridBagConstraints();
        gbc_txtfldFeedbackType.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldFeedbackType.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldFeedbackType.gridx = 3;
        gbc_txtfldFeedbackType.gridy = 4;
        detailsPanel.add(txtfldFeedbackType, gbc_txtfldFeedbackType);

        lblPriority = new JXLabel();
        lblPriority.setBorder(new EmptyBorder(0, 0, 0, 10));
        lblPriority.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblPriority.setText("Priority");
        GridBagConstraints gbc_lblPriority = new GridBagConstraints();
        gbc_lblPriority.anchor = GridBagConstraints.WEST;
        gbc_lblPriority.insets = new Insets(0, 0, 5, 5);
        gbc_lblPriority.gridx = 0;
        gbc_lblPriority.gridy = 5;
        detailsPanel.add(lblPriority, gbc_lblPriority);

        textfldPriority = new JXTextField();
        textfldPriority.setOpaque(false);
        textfldPriority.setBackground(new Color(0, 0, 0, 0));
        textfldPriority.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        textfldPriority.setEditable(false);
        GridBagConstraints gbc_txtfldPriority = new GridBagConstraints();
        gbc_txtfldPriority.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldPriority.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldPriority.gridx = 1;
        gbc_txtfldPriority.gridy = 5;
        detailsPanel.add(textfldPriority, gbc_txtfldPriority);
                
                        lblEnviroment = new JXLabel();
                        lblEnviroment.setBorder(new EmptyBorder(0, 20, 0, 10));
                        lblEnviroment.setFont(new Font("Tahoma", Font.BOLD, 11));
                        lblEnviroment.setText("Enviroment");
                        GridBagConstraints gbc_lblEnviroment = new GridBagConstraints();
                        gbc_lblEnviroment.anchor = GridBagConstraints.WEST;
                        gbc_lblEnviroment.insets = new Insets(0, 0, 5, 5);
                        gbc_lblEnviroment.gridx = 2;
                        gbc_lblEnviroment.gridy = 5;
                        detailsPanel.add(lblEnviroment, gbc_lblEnviroment);
                
                        txtfldEnviroment = new JXTextField();
                        txtfldEnviroment.setOpaque(false);
                        txtfldEnviroment.setBackground(new Color(0, 0, 0, 0));
                        txtfldEnviroment.setEditable(false);
                        txtfldEnviroment.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
                        GridBagConstraints gbc_txtfldEnviroment = new GridBagConstraints();
                        gbc_txtfldEnviroment.insets = new Insets(0, 0, 5, 0);
                        gbc_txtfldEnviroment.fill = GridBagConstraints.HORIZONTAL;
                        gbc_txtfldEnviroment.gridx = 3;
                        gbc_txtfldEnviroment.gridy = 5;
                        detailsPanel.add(txtfldEnviroment, gbc_txtfldEnviroment);
        
                lblPhase = new JXLabel();
                lblPhase.setBorder(new EmptyBorder(0, 0, 0, 10));
                lblPhase.setFont(new Font("Tahoma", Font.BOLD, 11));
                lblPhase.setText("Phase");
                GridBagConstraints gbc_lblPhase = new GridBagConstraints();
                gbc_lblPhase.anchor = GridBagConstraints.WEST;
                gbc_lblPhase.insets = new Insets(0, 0, 5, 5);
                gbc_lblPhase.gridx = 0;
                gbc_lblPhase.gridy = 6;
                detailsPanel.add(lblPhase, gbc_lblPhase);
        
                comboBoxPhase = new JComboBox();
                comboBoxPhase.setBorder(new EmptyBorder(0, 0, 0, 0));
                comboBoxPhase.setModel(new DefaultComboBoxModel(phases.keySet().toArray()));
                GridBagConstraints gbc_comboBox = new GridBagConstraints();
                gbc_comboBox.insets = new Insets(0, 0, 5, 5);
                gbc_comboBox.fill = GridBagConstraints.HORIZONTAL;
                gbc_comboBox.gridx = 1;
                gbc_comboBox.gridy = 6;
                detailsPanel.add(comboBoxPhase, gbc_comboBox);
        
        lblAppModule = new JXLabel();
        lblAppModule.setText("Application Module");
        lblAppModule.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblAppModule.setBorder(new EmptyBorder(0, 20, 0, 10));
        GridBagConstraints gbc_lblAppModule = new GridBagConstraints();
        gbc_lblAppModule.anchor = GridBagConstraints.SOUTHEAST;
        gbc_lblAppModule.insets = new Insets(0, 0, 5, 5);
        gbc_lblAppModule.gridx = 2;
        gbc_lblAppModule.gridy = 6;
        detailsPanel.add(lblAppModule, gbc_lblAppModule);
        
        txtfldAppModules = new JXTextField();
        txtfldAppModules.setOpaque(false);
        txtfldAppModules.setEditable(false);
        txtfldAppModules.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldAppModules.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldAppModules = new GridBagConstraints();
        gbc_txtfldAppModules.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldAppModules.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldAppModules.gridx = 3;
        gbc_txtfldAppModules.gridy = 6;
        detailsPanel.add(txtfldAppModules, gbc_txtfldAppModules);
        
        invizibleSpacerL = new JXTextField();
        invizibleSpacerL.setOpaque(false);
        invizibleSpacerL.setEditable(false);
        invizibleSpacerL.setBorder(new EmptyBorder(5, 0, 5, 0));
        invizibleSpacerL.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_invizibleSpacerL = new GridBagConstraints();
        gbc_invizibleSpacerL.insets = new Insets(0, 0, 5, 5);
        gbc_invizibleSpacerL.fill = GridBagConstraints.HORIZONTAL;
        gbc_invizibleSpacerL.gridx = 1;
        gbc_invizibleSpacerL.gridy = 7;
        detailsPanel.add(invizibleSpacerL, gbc_invizibleSpacerL);
        
        invizibleSpacerR = new JXTextField();
        invizibleSpacerR.setOpaque(false);
        invizibleSpacerR.setEditable(false);
        invizibleSpacerR.setBorder(new EmptyBorder(5, 0, 5, 0));
        invizibleSpacerR.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_invizibleSpacerR = new GridBagConstraints();
        gbc_invizibleSpacerR.insets = new Insets(0, 0, 5, 0);
        gbc_invizibleSpacerR.fill = GridBagConstraints.HORIZONTAL;
        gbc_invizibleSpacerR.gridx = 3;
        gbc_invizibleSpacerR.gridy = 7;
        detailsPanel.add(invizibleSpacerR, gbc_invizibleSpacerR);

        lblRelease = new JXLabel();
        lblRelease.setBorder(new EmptyBorder(0, 0, 0, 10));
        lblRelease.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblRelease.setText("Release");
        GridBagConstraints gbc_lblRelease = new GridBagConstraints();
        gbc_lblRelease.anchor = GridBagConstraints.WEST;
        gbc_lblRelease.insets = new Insets(0, 0, 5, 5);
        gbc_lblRelease.gridx = 0;
        gbc_lblRelease.gridy = 8;
        detailsPanel.add(lblRelease, gbc_lblRelease);

        txtfldRelease = new JXTextField();
        txtfldRelease.setOpaque(false);
        txtfldRelease.setBackground(new Color(0, 0, 0, 0));
        txtfldRelease.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldRelease.setEditable(false);
        GridBagConstraints gbc_txtfldIdeIntegration = new GridBagConstraints();
        gbc_txtfldIdeIntegration.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldIdeIntegration.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldIdeIntegration.gridx = 1;
        gbc_txtfldIdeIntegration.gridy = 8;
        detailsPanel.add(txtfldRelease, gbc_txtfldIdeIntegration);

        lblDetectedInPush = new JXLabel();
        lblDetectedInPush.setBorder(new EmptyBorder(0, 20, 0, 10));
        lblDetectedInPush.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDetectedInPush.setText("Detected in Build");
        GridBagConstraints gbc_lblDetectedInPush = new GridBagConstraints();
        gbc_lblDetectedInPush.anchor = GridBagConstraints.WEST;
        gbc_lblDetectedInPush.insets = new Insets(0, 0, 5, 5);
        gbc_lblDetectedInPush.gridx = 2;
        gbc_lblDetectedInPush.gridy = 8;
        detailsPanel.add(lblDetectedInPush, gbc_lblDetectedInPush);

        textfldDetectedInPush = new JXTextField();
        textfldDetectedInPush.setOpaque(false);
        textfldDetectedInPush.setBackground(new Color(0, 0, 0, 0));
        textfldDetectedInPush.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        textfldDetectedInPush.setEditable(false);
        GridBagConstraints gbc_textField_2 = new GridBagConstraints();
        gbc_textField_2.insets = new Insets(0, 0, 5, 0);
        gbc_textField_2.fill = GridBagConstraints.HORIZONTAL;
        gbc_textField_2.gridx = 3;
        gbc_textField_2.gridy = 8;
        detailsPanel.add(textfldDetectedInPush, gbc_textField_2);
        
                lblSprint = new JXLabel();
                lblSprint.setBorder(new EmptyBorder(0, 0, 0, 10));
                lblSprint.setFont(new Font("Tahoma", Font.BOLD, 11));
                lblSprint.setText("Sprint");
                GridBagConstraints gbc_lblSprint = new GridBagConstraints();
                gbc_lblSprint.anchor = GridBagConstraints.WEST;
                gbc_lblSprint.insets = new Insets(0, 0, 5, 5);
                gbc_lblSprint.gridx = 0;
                gbc_lblSprint.gridy = 9;
                detailsPanel.add(lblSprint, gbc_lblSprint);
        
                txtfldSprint = new JXTextField();
                txtfldSprint.setOpaque(false);
                txtfldSprint.setBackground(new Color(0, 0, 0, 0));
                txtfldSprint.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
                txtfldSprint.setEditable(false);
                GridBagConstraints gbc_txtfldMoreInfo = new GridBagConstraints();
                gbc_txtfldMoreInfo.insets = new Insets(0, 0, 5, 5);
                gbc_txtfldMoreInfo.fill = GridBagConstraints.HORIZONTAL;
                gbc_txtfldMoreInfo.gridx = 1;
                gbc_txtfldMoreInfo.gridy = 9;
                detailsPanel.add(txtfldSprint, gbc_txtfldMoreInfo);

        lblDetectedInRelease = new JXLabel();
        lblDetectedInRelease.setBorder(new EmptyBorder(0, 20, 0, 10));
        lblDetectedInRelease.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDetectedInRelease.setText("Detected in release");
        GridBagConstraints gbc_lblDetectedInRelease = new GridBagConstraints();
        gbc_lblDetectedInRelease.anchor = GridBagConstraints.WEST;
        gbc_lblDetectedInRelease.insets = new Insets(0, 0, 5, 5);
        gbc_lblDetectedInRelease.gridx = 2;
        gbc_lblDetectedInRelease.gridy = 9;
        detailsPanel.add(lblDetectedInRelease, gbc_lblDetectedInRelease);

        txtfldDetectedInRelease = new JXTextField();
        txtfldDetectedInRelease.setOpaque(false);
        txtfldDetectedInRelease.setBackground(new Color(0, 0, 0, 0));
        txtfldDetectedInRelease.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldDetectedInRelease.setEditable(false);
        GridBagConstraints gbc_txtfldBlaBla_1 = new GridBagConstraints();
        gbc_txtfldBlaBla_1.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldBlaBla_1.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldBlaBla_1.gridx = 3;
        gbc_txtfldBlaBla_1.gridy = 9;
        detailsPanel.add(txtfldDetectedInRelease, gbc_txtfldBlaBla_1);
                                
                                        lblFixedInPush = new JXLabel();
                                        lblFixedInPush.setBorder(new EmptyBorder(0, 20, 0, 10));
                                        lblFixedInPush.setFont(new Font("Tahoma", Font.BOLD, 11));
                                        lblFixedInPush.setText("Fixed in Build");
                                        GridBagConstraints gbc_lblFixedInPush = new GridBagConstraints();
                                        gbc_lblFixedInPush.anchor = GridBagConstraints.WEST;
                                        gbc_lblFixedInPush.insets = new Insets(0, 0, 0, 5);
                                        gbc_lblFixedInPush.gridx = 2;
                                        gbc_lblFixedInPush.gridy = 10;
                                        detailsPanel.add(lblFixedInPush, gbc_lblFixedInPush);
                                
                                        textfldFixedInPush = new JXTextField();
                                        textfldFixedInPush.setOpaque(false);
                                        textfldFixedInPush.setBackground(new Color(0, 0, 0, 0));
                                        textfldFixedInPush.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
                                        textfldFixedInPush.setEditable(false);
                                        GridBagConstraints gbc_textField_3 = new GridBagConstraints();
                                        gbc_textField_3.fill = GridBagConstraints.HORIZONTAL;
                                        gbc_textField_3.gridx = 3;
                                        gbc_textField_3.gridy = 10;
                                        detailsPanel.add(textfldFixedInPush, gbc_textField_3);
                                        
                                        panel = new JXPanel();
                                        GridBagConstraints gbc_panel = new GridBagConstraints();
                                        gbc_panel.fill = GridBagConstraints.BOTH;
                                        gbc_panel.gridx = 0;
                                        gbc_panel.gridy = 3;
                                        parentPanel.add(panel, gbc_panel);
                                        GridBagLayout gbl_panel = new GridBagLayout();
                                        gbl_panel.columnWidths = new int[]{0, 0, 0};
                                        gbl_panel.rowHeights = new int[]{0, 0};
                                        gbl_panel.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
                                        gbl_panel.rowWeights = new double[]{0.0, Double.MIN_VALUE};
                                        panel.setLayout(gbl_panel);
                                        
                                        lblAttachments = new JXLabel();
                                        lblAttachments.setBorder(new EmptyBorder(10, 7, 0, 10));
                                        lblAttachments.setFont(new Font("Tahoma", Font.BOLD, 11));
                                        lblAttachments.setText("Attachments");
                                        GridBagConstraints gbc_lblAttachments = new GridBagConstraints();
                                        gbc_lblAttachments.insets = new Insets(0, 0, 0, 5);
                                        gbc_lblAttachments.gridx = 0;
                                        gbc_lblAttachments.gridy = 0;
                                        panel.add(lblAttachments, gbc_lblAttachments);
                                        
                                        lblLinksToFile = new JXLabel();
                                        lblLinksToFile.setBorder(new EmptyBorder(10, 0, 0, 0));
                                        lblLinksToFile.setLineWrap(true);
                                        lblLinksToFile.setText("No attachments");
                                        GridBagConstraints gbc_lblLinksToFile = new GridBagConstraints();
                                        gbc_lblLinksToFile.fill = GridBagConstraints.HORIZONTAL;
                                        gbc_lblLinksToFile.gridx = 1;
                                        gbc_lblLinksToFile.gridy = 0;
                                        panel.add(lblLinksToFile, gbc_lblLinksToFile);
    }

    public void setTxtfldDescription(String description) {
        this.txtfldDescription.setText(description);
    }

    public void setTxtfldFeature(String feature) {
        this.txtfldFeature.setText(feature);
    }

    public void setTxtfldQaOwner(String qaOwner) {
        this.txtfldQaOwner.setText(qaOwner);
    }

    public void setTxtfldTeam(String team) {
        this.txtfldTeam.setText(team);
    }

    public void setTextfldPriority(String priority) {
        this.textfldPriority.setText(priority);
    }

    public void setTxtfldRelease(String release) {
        this.txtfldRelease.setText(release);
    }

    public void setTextfldFixedInPush(String fixedInPush) {
        this.textfldFixedInPush.setText(fixedInPush);
    }

    public void setTxtfldSprint(String sprint) {
        this.txtfldSprint.setText(sprint);
    }

    public void setTxtfldDetectedBy(String detectedBy) {
        this.txtfldDetectedBy.setText(detectedBy);
    }

    public void setTxtfldSeverity(String severity) {
        this.txtfldSeverity.setText(severity);
    }

    public void setTxtfldGroup(String group) {
        this.txtfldGroup.setText(group);
    }

    public void setTxtfldEnviroment(String enviroment) {
        this.txtfldEnviroment.setText(enviroment);
    }

    public void setTxtfldFeedbackType(String feedbackType) {
        this.txtfldFeedbackType.setText(feedbackType);
    }

    public void setTextfldStoryPoints(String storyPoints) {
        this.textfldStoryPoints.setText(storyPoints);
    }

    public void setTextfldDetectedInPush(String detectedInPush) {
        this.textfldDetectedInPush.setText(detectedInPush);
    }

    public void setTxtfldDetectedInRelease(String detectedInRelease) {
        this.txtfldDetectedInRelease.setText(detectedInRelease);
    }

    public String getComboBoxPhase() {
        return comboBoxPhase.getSelectedItem().toString();
    }

    void setComboBoxPhase(String phaseName) {
        phases.entrySet().stream().filter(entry -> phaseName.equals(entry.getKey())).forEach(entry -> {
            this.comboBoxPhase.setSelectedIndex(1);
        });
    }

	public String getTxtfldBlocked() {
		return txtfldBlocked.getText();
	}

	public void setTxtfldBlocked(String txtfldBlocked) {
		this.txtfldBlocked.setText(txtfldBlocked);
	}

	public String getTxtfldBlockedReason() {
		return txtfldBlockedReason.getText();
	}

	public void setTxtfldBlockedReason(String txtfldBlockedReason) {
		this.txtfldBlockedReason.setText(txtfldBlockedReason);
	}

	public String getTxtfldAppModules() {
		return txtfldAppModules.getText();
	}

	public void setTxtfldAppModules(String txtfldAppModules) {
		this.txtfldAppModules.setText(txtfldAppModules);
	}
    
}

	
