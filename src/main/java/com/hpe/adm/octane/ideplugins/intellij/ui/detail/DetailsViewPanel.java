package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextArea;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;

public class DetailsViewPanel extends JPanel {

    private JXPanel detailsPanel;

    private JXLabel lblDescription;
    private JXTextArea txtfldDescription;

    private JXLabel lblFeature;
    private JXTextField txtfldFeature;

    private JXLabel lblQaOwner;
    private JXTextField txtfldQaOwner;

    private JXLabel lblHolder;
    private JXTextField txtfldHolder;

    private JXLabel lblTeam;
    private JXTextField txtfldTeam;

    private JXLabel lblCustomer;
    private JXTextField txtfldCustomer;

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

    public DetailsViewPanel() {
        setBounds(100, 100, 814, 350);
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
        gbl_parentPanel.rowHeights = new int[]{0, 0, 0, 0};
        gbl_parentPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_parentPanel.rowWeights = new double[]{0.0, 0.0, 1.0, Double.MIN_VALUE};
        parentPanel.setLayout(gbl_parentPanel);

        lblDescription = new JXLabel();
        lblDescription.setBorder(new EmptyBorder(5, 5, 0, 0));
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
        txtfldDescription.setBorder(new EmptyBorder(5, 5, 0, 0));
        txtfldDescription.setLineWrap(true);
        txtfldDescription.setText("Description");
        GridBagConstraints gbc_txtrDescription = new GridBagConstraints();
        gbc_txtrDescription.insets = new Insets(0, 0, 5, 0);
        gbc_txtrDescription.fill = GridBagConstraints.BOTH;
        gbc_txtrDescription.gridx = 0;
        gbc_txtrDescription.gridy = 1;
        parentPanel.add(txtfldDescription, gbc_txtrDescription);

        detailsPanel = new JXPanel();
        detailsPanel.setOpaque(false);
        detailsPanel.setBorder(new EmptyBorder(2, 2, 2, 2));
        GridBagConstraints gbc_detailsPanel = new GridBagConstraints();
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
        lblFeature.setBorder(new EmptyBorder(0, 5, 0, 5));
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
        txtfldFeature.setText("Feature");
        GridBagConstraints gbc_txtfldFeature = new GridBagConstraints();
        gbc_txtfldFeature.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldFeature.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldFeature.gridx = 1;
        gbc_txtfldFeature.gridy = 0;
        detailsPanel.add(txtfldFeature, gbc_txtfldFeature);

        lblDetectedBy = new JXLabel();
        lblDetectedBy.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblDetectedBy.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDetectedBy.setText("Detected by");
        GridBagConstraints gbc_lblAsd = new GridBagConstraints();
        gbc_lblAsd.insets = new Insets(0, 0, 5, 5);
        gbc_lblAsd.anchor = GridBagConstraints.WEST;
        gbc_lblAsd.gridx = 2;
        gbc_lblAsd.gridy = 0;
        detailsPanel.add(lblDetectedBy, gbc_lblAsd);

        txtfldDetectedBy = new JXTextField();
        txtfldDetectedBy.setOpaque(false);
        txtfldDetectedBy.setBackground(new Color(0, 0, 0, 0));
        txtfldDetectedBy.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldDetectedBy.setEditable(false);
        txtfldDetectedBy.setText("Donald Trump");
        GridBagConstraints gbc_txtfldDonaldTrump = new GridBagConstraints();
        gbc_txtfldDonaldTrump.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldDonaldTrump.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldDonaldTrump.gridx = 3;
        gbc_txtfldDonaldTrump.gridy = 0;
        detailsPanel.add(txtfldDetectedBy, gbc_txtfldDonaldTrump);

        lblQaOwner = new JXLabel();
        lblQaOwner.setBorder(new EmptyBorder(0, 5, 0, 5));
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
        txtfldQaOwner.setText("Donald Trump");
        GridBagConstraints gbc_txtfldDonaldTrump_1 = new GridBagConstraints();
        gbc_txtfldDonaldTrump_1.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldDonaldTrump_1.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldDonaldTrump_1.gridx = 1;
        gbc_txtfldDonaldTrump_1.gridy = 1;
        detailsPanel.add(txtfldQaOwner, gbc_txtfldDonaldTrump_1);

        lblSeverity = new JXLabel();
        lblSeverity.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblSeverity.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblSeverity.setText("Severity");
        GridBagConstraints gbc_lblSeverity = new GridBagConstraints();
        gbc_lblSeverity.anchor = GridBagConstraints.WEST;
        gbc_lblSeverity.insets = new Insets(0, 0, 5, 5);
        gbc_lblSeverity.gridx = 2;
        gbc_lblSeverity.gridy = 1;
        detailsPanel.add(lblSeverity, gbc_lblSeverity);

        txtfldSeverity = new JXTextField();
        txtfldSeverity.setOpaque(false);
        txtfldSeverity.setBackground(new Color(0, 0, 0, 0));
        txtfldSeverity.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldSeverity.setEditable(false);
        txtfldSeverity.setText("Very High");
        GridBagConstraints gbc_txtfldVeryHigh = new GridBagConstraints();
        gbc_txtfldVeryHigh.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldVeryHigh.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldVeryHigh.gridx = 3;
        gbc_txtfldVeryHigh.gridy = 1;
        detailsPanel.add(txtfldSeverity, gbc_txtfldVeryHigh);

        lblHolder = new JXLabel();
        lblHolder.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblHolder.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblHolder.setText("Holder");
        GridBagConstraints gbc_lblHolder = new GridBagConstraints();
        gbc_lblHolder.anchor = GridBagConstraints.WEST;
        gbc_lblHolder.insets = new Insets(0, 0, 5, 5);
        gbc_lblHolder.gridx = 0;
        gbc_lblHolder.gridy = 2;
        detailsPanel.add(lblHolder, gbc_lblHolder);

        txtfldHolder = new JXTextField();
        txtfldHolder.setOpaque(false);
        txtfldHolder.setBackground(new Color(0, 0, 0, 0));
        txtfldHolder.setEditable(false);
        txtfldHolder.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldHolder.setText("Push 1");
        GridBagConstraints gbc_txtfldPush = new GridBagConstraints();
        gbc_txtfldPush.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldPush.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldPush.gridx = 1;
        gbc_txtfldPush.gridy = 2;
        detailsPanel.add(txtfldHolder, gbc_txtfldPush);

        lblGroup = new JXLabel();
        lblGroup.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblGroup.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblGroup.setText("Group");
        GridBagConstraints gbc_lblGroup = new GridBagConstraints();
        gbc_lblGroup.anchor = GridBagConstraints.WEST;
        gbc_lblGroup.insets = new Insets(0, 0, 5, 5);
        gbc_lblGroup.gridx = 2;
        gbc_lblGroup.gridy = 2;
        detailsPanel.add(lblGroup, gbc_lblGroup);

        txtfldGroup = new JXTextField();
        txtfldGroup.setOpaque(false);
        txtfldGroup.setBackground(new Color(0, 0, 0, 0));
        txtfldGroup.setEditable(false);
        txtfldGroup.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldGroup.setText("AGM_ALL");
        GridBagConstraints gbc_txtfldAgmall = new GridBagConstraints();
        gbc_txtfldAgmall.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldAgmall.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldAgmall.gridx = 3;
        gbc_txtfldAgmall.gridy = 2;
        detailsPanel.add(txtfldGroup, gbc_txtfldAgmall);

        lblTeam = new JXLabel();
        lblTeam.setBorder(new EmptyBorder(0, 5, 0, 5));
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
        txtfldTeam.setText("a lone wolf");
        GridBagConstraints gbc_txtfldALoneWolf = new GridBagConstraints();
        gbc_txtfldALoneWolf.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldALoneWolf.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldALoneWolf.gridx = 1;
        gbc_txtfldALoneWolf.gridy = 3;
        detailsPanel.add(txtfldTeam, gbc_txtfldALoneWolf);

        lblEnviroment = new JXLabel();
        lblEnviroment.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblEnviroment.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblEnviroment.setText("Enviroment");
        GridBagConstraints gbc_lblEnviroment = new GridBagConstraints();
        gbc_lblEnviroment.anchor = GridBagConstraints.WEST;
        gbc_lblEnviroment.insets = new Insets(0, 0, 5, 5);
        gbc_lblEnviroment.gridx = 2;
        gbc_lblEnviroment.gridy = 3;
        detailsPanel.add(lblEnviroment, gbc_lblEnviroment);

        txtfldEnviroment = new JXTextField();
        txtfldEnviroment.setOpaque(false);
        txtfldEnviroment.setBackground(new Color(0, 0, 0, 0));
        txtfldEnviroment.setEditable(false);
        txtfldEnviroment.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldEnviroment.setText("DEV");
        GridBagConstraints gbc_txtfldDev = new GridBagConstraints();
        gbc_txtfldDev.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldDev.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldDev.gridx = 3;
        gbc_txtfldDev.gridy = 3;
        detailsPanel.add(txtfldEnviroment, gbc_txtfldDev);

        lblCustomer = new JXLabel();
        lblCustomer.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblCustomer.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblCustomer.setText("Customer");
        GridBagConstraints gbc_lblCustomer = new GridBagConstraints();
        gbc_lblCustomer.anchor = GridBagConstraints.WEST;
        gbc_lblCustomer.insets = new Insets(0, 0, 5, 5);
        gbc_lblCustomer.gridx = 0;
        gbc_lblCustomer.gridy = 4;
        detailsPanel.add(lblCustomer, gbc_lblCustomer);

        txtfldCustomer = new JXTextField();
        txtfldCustomer.setOpaque(false);
        txtfldCustomer.setBackground(new Color(0, 0, 0, 0));
        txtfldCustomer.setEditable(false);
        txtfldCustomer.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldCustomer.setText("bla bla");
        GridBagConstraints gbc_txtfldBlaBla = new GridBagConstraints();
        gbc_txtfldBlaBla.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldBlaBla.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldBlaBla.gridx = 1;
        gbc_txtfldBlaBla.gridy = 4;
        detailsPanel.add(txtfldCustomer, gbc_txtfldBlaBla);

        lblFeedbackType = new JXLabel();
        lblFeedbackType.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblFeedbackType.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblFeedbackType.setText("Feedback type");
        GridBagConstraints gbc_lblFeedbackType = new GridBagConstraints();
        gbc_lblFeedbackType.anchor = GridBagConstraints.WEST;
        gbc_lblFeedbackType.insets = new Insets(0, 0, 5, 5);
        gbc_lblFeedbackType.gridx = 2;
        gbc_lblFeedbackType.gridy = 4;
        detailsPanel.add(lblFeedbackType, gbc_lblFeedbackType);

        txtfldFeedbackType = new JXTextField();
        txtfldFeedbackType.setOpaque(false);
        txtfldFeedbackType.setBackground(new Color(0, 0, 0, 0));
        txtfldFeedbackType.setEditable(false);
        txtfldFeedbackType.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldFeedbackType.setText("Defect");
        GridBagConstraints gbc_txtfldDefect = new GridBagConstraints();
        gbc_txtfldDefect.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldDefect.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldDefect.gridx = 3;
        gbc_txtfldDefect.gridy = 4;
        detailsPanel.add(txtfldFeedbackType, gbc_txtfldDefect);

        lblPriority = new JXLabel();
        lblPriority.setBorder(new EmptyBorder(0, 5, 0, 5));
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
        textfldPriority.setText("1");
        GridBagConstraints gbc_textField = new GridBagConstraints();
        gbc_textField.insets = new Insets(0, 0, 5, 5);
        gbc_textField.fill = GridBagConstraints.HORIZONTAL;
        gbc_textField.gridx = 1;
        gbc_textField.gridy = 5;
        detailsPanel.add(textfldPriority, gbc_textField);

        lblStoryPoints = new JXLabel();
        lblStoryPoints.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblStoryPoints.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblStoryPoints.setText("Story Points");
        GridBagConstraints gbc_lblStoryPoints = new GridBagConstraints();
        gbc_lblStoryPoints.anchor = GridBagConstraints.WEST;
        gbc_lblStoryPoints.insets = new Insets(0, 0, 5, 5);
        gbc_lblStoryPoints.gridx = 2;
        gbc_lblStoryPoints.gridy = 5;
        detailsPanel.add(lblStoryPoints, gbc_lblStoryPoints);

        textfldStoryPoints = new JXTextField();
        textfldStoryPoints.setOpaque(false);
        textfldStoryPoints.setBackground(new Color(0, 0, 0, 0));
        textfldStoryPoints.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        textfldStoryPoints.setEditable(false);
        textfldStoryPoints.setText("5");
        GridBagConstraints gbc_textField_1 = new GridBagConstraints();
        gbc_textField_1.insets = new Insets(0, 0, 5, 0);
        gbc_textField_1.fill = GridBagConstraints.HORIZONTAL;
        gbc_textField_1.gridx = 3;
        gbc_textField_1.gridy = 5;
        detailsPanel.add(textfldStoryPoints, gbc_textField_1);

        lblPhase = new JXLabel();
        lblPhase.setBorder(new EmptyBorder(0, 5, 0, 5));
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
        comboBoxPhase.setModel(new DefaultComboBoxModel(new String[]{"Open", "In Progress", "Done"}));
        GridBagConstraints gbc_comboBox = new GridBagConstraints();
        gbc_comboBox.insets = new Insets(0, 0, 5, 5);
        gbc_comboBox.fill = GridBagConstraints.HORIZONTAL;
        gbc_comboBox.gridx = 1;
        gbc_comboBox.gridy = 6;
        detailsPanel.add(comboBoxPhase, gbc_comboBox);

        lblRelease = new JXLabel();
        lblRelease.setBorder(new EmptyBorder(0, 5, 0, 5));
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
        txtfldRelease.setText("IDE Integration");
        GridBagConstraints gbc_txtfldIdeIntegration = new GridBagConstraints();
        gbc_txtfldIdeIntegration.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldIdeIntegration.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldIdeIntegration.gridx = 1;
        gbc_txtfldIdeIntegration.gridy = 8;
        detailsPanel.add(txtfldRelease, gbc_txtfldIdeIntegration);

        lblDetectedInPush = new JXLabel();
        lblDetectedInPush.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblDetectedInPush.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDetectedInPush.setText("Detected in Push");
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
        textfldDetectedInPush.setText("12.34.56");
        GridBagConstraints gbc_textField_2 = new GridBagConstraints();
        gbc_textField_2.insets = new Insets(0, 0, 5, 0);
        gbc_textField_2.fill = GridBagConstraints.HORIZONTAL;
        gbc_textField_2.gridx = 3;
        gbc_textField_2.gridy = 8;
        detailsPanel.add(textfldDetectedInPush, gbc_textField_2);

        lblFixedInPush = new JXLabel();
        lblFixedInPush.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblFixedInPush.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblFixedInPush.setText("Fixed in Push");
        GridBagConstraints gbc_lblFixedInPush = new GridBagConstraints();
        gbc_lblFixedInPush.anchor = GridBagConstraints.WEST;
        gbc_lblFixedInPush.insets = new Insets(0, 0, 5, 5);
        gbc_lblFixedInPush.gridx = 0;
        gbc_lblFixedInPush.gridy = 9;
        detailsPanel.add(lblFixedInPush, gbc_lblFixedInPush);

        textfldFixedInPush = new JXTextField();
        textfldFixedInPush.setOpaque(false);
        textfldFixedInPush.setBackground(new Color(0, 0, 0, 0));
        textfldFixedInPush.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        textfldFixedInPush.setEditable(false);
        textfldFixedInPush.setText("12.34.56");
        GridBagConstraints gbc_textField_3 = new GridBagConstraints();
        gbc_textField_3.insets = new Insets(0, 0, 5, 5);
        gbc_textField_3.fill = GridBagConstraints.HORIZONTAL;
        gbc_textField_3.gridx = 1;
        gbc_textField_3.gridy = 9;
        detailsPanel.add(textfldFixedInPush, gbc_textField_3);

        lblDetectedInRelease = new JXLabel();
        lblDetectedInRelease.setBorder(new EmptyBorder(0, 5, 0, 5));
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
        txtfldDetectedInRelease.setText("bla bla");
        GridBagConstraints gbc_txtfldBlaBla_1 = new GridBagConstraints();
        gbc_txtfldBlaBla_1.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldBlaBla_1.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldBlaBla_1.gridx = 3;
        gbc_txtfldBlaBla_1.gridy = 9;
        detailsPanel.add(txtfldDetectedInRelease, gbc_txtfldBlaBla_1);

        lblSprint = new JXLabel();
        lblSprint.setBorder(new EmptyBorder(0, 5, 0, 5));
        lblSprint.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblSprint.setText("Sprint");
        GridBagConstraints gbc_lblSprint = new GridBagConstraints();
        gbc_lblSprint.anchor = GridBagConstraints.WEST;
        gbc_lblSprint.insets = new Insets(0, 0, 0, 5);
        gbc_lblSprint.gridx = 0;
        gbc_lblSprint.gridy = 10;
        detailsPanel.add(lblSprint, gbc_lblSprint);

        txtfldSprint = new JXTextField();
        txtfldSprint.setOpaque(false);
        txtfldSprint.setBackground(new Color(0, 0, 0, 0));
        txtfldSprint.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldSprint.setEditable(false);
        txtfldSprint.setText("more info");
        GridBagConstraints gbc_txtfldMoreInfo = new GridBagConstraints();
        gbc_txtfldMoreInfo.insets = new Insets(0, 0, 0, 5);
        gbc_txtfldMoreInfo.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldMoreInfo.gridx = 1;
        gbc_txtfldMoreInfo.gridy = 10;
        detailsPanel.add(txtfldSprint, gbc_txtfldMoreInfo);
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

    public void setTxtfldHolder(String holder) {
        this.txtfldHolder.setText(holder);
    }

    public void setTxtfldTeam(String team) {
        this.txtfldTeam.setText(team);
    }

    public void setTxtfldCustomer(String customer) {
        this.txtfldCustomer.setText(customer);
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

    public JComboBox getComboBoxPhase() {
        return comboBoxPhase;
    }

    public void setComboBoxPhase(JComboBox comboBoxPhase) {
        this.comboBoxPhase = comboBoxPhase;
    }
}

	
