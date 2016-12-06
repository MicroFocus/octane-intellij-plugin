package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

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

public class TestDetailsPanel extends JPanel {
    private JXPanel rootPanel;
    private JLabel lblName;
    private JXLabel lblDescription;
    private JXTextArea txtfldDescription;
    private JXPanel panel_1;
    private JXLabel lblAppModules;
    private JXTextField txtfldAppModules;
    private JXLabel lblCreated;
    private JXTextField txtfldCreated;
    private JXLabel lblDesigner;
    private JXTextField txtfldDesigner;
    private JXLabel lblOwner;
    private JXTextField txtfldOwner;
    private JXLabel lblManual;
    private JXTextField txtfldManual;
    private JXLabel lblFeature;
    private JXTextField txtfldFeature;
    private JXLabel lblRunInReleases;
    private JXTextField textRunInReleases;
    private JXLabel lblEstimatedDuration;
    private JXTextField txtfldEstimetedDuration;
    private JXLabel lblTestType;
    private JXTextField txtfldTestType;
    private JXLabel lblRequirementCoverage;
    private JXTextField txtfldRequirementCoverage;
    private JXLabel lblTestingToolType;
    private JXTextField txtfldTestingToolType;
    private JXLabel lblRunSet;
    private JXTextField txtfldRunSet;
    private JXLabel lblLastRuns;
    private JXTextField txtfldLastRuns;
    private JXLabel lblContent;
    private JXTextField txtfldContent;
    private JXLabel lblPhase;
    private JComboBox comboBoxPhase;
    private JXLabel lblCoveredContent;
    private JXTextField txtfldCoveredContent;
    private JXPanel nameAndIconPanel;

    public TestDetailsPanel(Entity entity) {
        setBorder(null);
        setBounds(100, 100, 900, 291);
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
        nameAndIconPanel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        FlowLayout flowLayout = (FlowLayout) nameAndIconPanel.getLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        GridBagConstraints gbc_nameAndIconPanel = new GridBagConstraints();
        gbc_nameAndIconPanel.insets = new Insets(0, 0, 5, 0);
        gbc_nameAndIconPanel.fill = GridBagConstraints.BOTH;
        gbc_nameAndIconPanel.gridx = 0;
        gbc_nameAndIconPanel.gridy = 0;
        rootPanel.add(nameAndIconPanel, gbc_nameAndIconPanel);

        lblName = new JXLabel();
        lblName.setFont(new Font("Tahoma", Font.BOLD, 12));
        nameAndIconPanel.add(lblName);

        lblName.setText("Name");
        lblName.setBorder(null);

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

        panel_1 = new JXPanel();
        panel_1.setBorder(new EmptyBorder(10, 10, 2, 10));
        GridBagConstraints gbc_panel_1 = new GridBagConstraints();
        gbc_panel_1.fill = GridBagConstraints.BOTH;
        gbc_panel_1.gridx = 0;
        gbc_panel_1.gridy = 3;
        rootPanel.add(panel_1, gbc_panel_1);
        GridBagLayout gbl_panel_1 = new GridBagLayout();
        gbl_panel_1.columnWidths = new int[]{0, 0, 0, 0, 0};
        gbl_panel_1.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_panel_1.columnWeights = new double[]{0.0, 1.0, 0.0, 1.0, Double.MIN_VALUE};
        gbl_panel_1.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        panel_1.setLayout(gbl_panel_1);

        lblAppModules = new JXLabel();
        lblAppModules.setText("Application Module");
        lblAppModules.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblAppModules.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblAppModules = new GridBagConstraints();
        gbc_lblAppModules.anchor = GridBagConstraints.WEST;
        gbc_lblAppModules.insets = new Insets(0, 0, 5, 5);
        gbc_lblAppModules.gridx = 0;
        gbc_lblAppModules.gridy = 0;
        panel_1.add(lblAppModules, gbc_lblAppModules);

        txtfldAppModules = new JXTextField();
        txtfldAppModules.setOpaque(false);
        txtfldAppModules.setEditable(false);
        txtfldAppModules.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldAppModules.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldAppModules = new GridBagConstraints();
        gbc_txtfldAppModules.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldAppModules.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldAppModules.gridx = 1;
        gbc_txtfldAppModules.gridy = 0;
        panel_1.add(txtfldAppModules, gbc_txtfldAppModules);

        lblCreated = new JXLabel();
        lblCreated.setText("Created");
        lblCreated.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblCreated.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblCreated = new GridBagConstraints();
        gbc_lblCreated.anchor = GridBagConstraints.WEST;
        gbc_lblCreated.insets = new Insets(0, 0, 5, 5);
        gbc_lblCreated.gridx = 2;
        gbc_lblCreated.gridy = 0;
        panel_1.add(lblCreated, gbc_lblCreated);

        txtfldCreated = new JXTextField();
        txtfldCreated.setOpaque(false);
        txtfldCreated.setEditable(false);
        txtfldCreated.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldCreated.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldCreated = new GridBagConstraints();
        gbc_txtfldCreated.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldCreated.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldCreated.gridx = 3;
        gbc_txtfldCreated.gridy = 0;
        panel_1.add(txtfldCreated, gbc_txtfldCreated);

        lblDesigner = new JXLabel();
        lblDesigner.setText("Designer");
        lblDesigner.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDesigner.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblDesigner = new GridBagConstraints();
        gbc_lblDesigner.anchor = GridBagConstraints.WEST;
        gbc_lblDesigner.insets = new Insets(0, 0, 5, 5);
        gbc_lblDesigner.gridx = 0;
        gbc_lblDesigner.gridy = 1;
        panel_1.add(lblDesigner, gbc_lblDesigner);

        txtfldDesigner = new JXTextField();
        txtfldDesigner.setOpaque(false);
        txtfldDesigner.setEditable(false);
        txtfldDesigner.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldDesigner.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldDesigner = new GridBagConstraints();
        gbc_txtfldDesigner.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldDesigner.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldDesigner.gridx = 1;
        gbc_txtfldDesigner.gridy = 1;
        panel_1.add(txtfldDesigner, gbc_txtfldDesigner);

        lblOwner = new JXLabel();
        lblOwner.setText("Owner");
        lblOwner.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblOwner.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblOwner = new GridBagConstraints();
        gbc_lblOwner.anchor = GridBagConstraints.WEST;
        gbc_lblOwner.insets = new Insets(0, 0, 5, 5);
        gbc_lblOwner.gridx = 2;
        gbc_lblOwner.gridy = 1;
        panel_1.add(lblOwner, gbc_lblOwner);

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
        panel_1.add(txtfldOwner, gbc_txtfldOwner);

        lblManual = new JXLabel();
        lblManual.setText("Manual");
        lblManual.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblManual.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblManual = new GridBagConstraints();
        gbc_lblManual.anchor = GridBagConstraints.WEST;
        gbc_lblManual.insets = new Insets(0, 0, 5, 5);
        gbc_lblManual.gridx = 0;
        gbc_lblManual.gridy = 2;
        panel_1.add(lblManual, gbc_lblManual);

        txtfldManual = new JXTextField();
        txtfldManual.setOpaque(false);
        txtfldManual.setEditable(false);
        txtfldManual.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldManual.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldManual = new GridBagConstraints();
        gbc_txtfldManual.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldManual.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldManual.gridx = 1;
        gbc_txtfldManual.gridy = 2;
        panel_1.add(txtfldManual, gbc_txtfldManual);

        lblFeature = new JXLabel();
        lblFeature.setText("Feature");
        lblFeature.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblFeature.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblFeature = new GridBagConstraints();
        gbc_lblFeature.anchor = GridBagConstraints.WEST;
        gbc_lblFeature.insets = new Insets(0, 0, 5, 5);
        gbc_lblFeature.gridx = 2;
        gbc_lblFeature.gridy = 2;
        panel_1.add(lblFeature, gbc_lblFeature);

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
        panel_1.add(txtfldFeature, gbc_txtfldFeature);

        lblRunInReleases = new JXLabel();
        lblRunInReleases.setText("Run in Releases");
        lblRunInReleases.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblRunInReleases.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblRunInReleases = new GridBagConstraints();
        gbc_lblRunInReleases.anchor = GridBagConstraints.WEST;
        gbc_lblRunInReleases.insets = new Insets(0, 0, 5, 5);
        gbc_lblRunInReleases.gridx = 0;
        gbc_lblRunInReleases.gridy = 3;
        panel_1.add(lblRunInReleases, gbc_lblRunInReleases);

        textRunInReleases = new JXTextField();
        textRunInReleases.setOpaque(false);
        textRunInReleases.setEditable(false);
        textRunInReleases.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        textRunInReleases.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_textRunInReleases = new GridBagConstraints();
        gbc_textRunInReleases.fill = GridBagConstraints.HORIZONTAL;
        gbc_textRunInReleases.insets = new Insets(0, 0, 5, 5);
        gbc_textRunInReleases.gridx = 1;
        gbc_textRunInReleases.gridy = 3;
        panel_1.add(textRunInReleases, gbc_textRunInReleases);

        lblEstimatedDuration = new JXLabel();
        lblEstimatedDuration.setText("Estimated duration");
        lblEstimatedDuration.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblEstimatedDuration.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblEstimatedDuration = new GridBagConstraints();
        gbc_lblEstimatedDuration.anchor = GridBagConstraints.WEST;
        gbc_lblEstimatedDuration.insets = new Insets(0, 0, 5, 5);
        gbc_lblEstimatedDuration.gridx = 2;
        gbc_lblEstimatedDuration.gridy = 3;
        panel_1.add(lblEstimatedDuration, gbc_lblEstimatedDuration);

        txtfldEstimetedDuration = new JXTextField();
        txtfldEstimetedDuration.setOpaque(false);
        txtfldEstimetedDuration.setEditable(false);
        txtfldEstimetedDuration.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldEstimetedDuration.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldEstimetedDuration = new GridBagConstraints();
        gbc_txtfldEstimetedDuration.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldEstimetedDuration.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldEstimetedDuration.gridx = 3;
        gbc_txtfldEstimetedDuration.gridy = 3;
        panel_1.add(txtfldEstimetedDuration, gbc_txtfldEstimetedDuration);

        lblTestType = new JXLabel();
        lblTestType.setText("Test type");
        lblTestType.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblTestType.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblTestType = new GridBagConstraints();
        gbc_lblTestType.anchor = GridBagConstraints.WEST;
        gbc_lblTestType.insets = new Insets(0, 0, 5, 5);
        gbc_lblTestType.gridx = 0;
        gbc_lblTestType.gridy = 4;
        panel_1.add(lblTestType, gbc_lblTestType);

        txtfldTestType = new JXTextField();
        txtfldTestType.setOpaque(false);
        txtfldTestType.setEditable(false);
        txtfldTestType.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldTestType.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldTestType = new GridBagConstraints();
        gbc_txtfldTestType.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldTestType.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldTestType.gridx = 1;
        gbc_txtfldTestType.gridy = 4;
        panel_1.add(txtfldTestType, gbc_txtfldTestType);

        lblRequirementCoverage = new JXLabel();
        lblRequirementCoverage.setText("Requirement coverage");
        lblRequirementCoverage.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblRequirementCoverage.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblRequirementCoverage = new GridBagConstraints();
        gbc_lblRequirementCoverage.anchor = GridBagConstraints.WEST;
        gbc_lblRequirementCoverage.insets = new Insets(0, 0, 5, 5);
        gbc_lblRequirementCoverage.gridx = 2;
        gbc_lblRequirementCoverage.gridy = 4;
        panel_1.add(lblRequirementCoverage, gbc_lblRequirementCoverage);

        txtfldRequirementCoverage = new JXTextField();
        txtfldRequirementCoverage.setOpaque(false);
        txtfldRequirementCoverage.setEditable(false);
        txtfldRequirementCoverage.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldRequirementCoverage.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldRequirementCoverage = new GridBagConstraints();
        gbc_txtfldRequirementCoverage.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldRequirementCoverage.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldRequirementCoverage.gridx = 3;
        gbc_txtfldRequirementCoverage.gridy = 4;
        panel_1.add(txtfldRequirementCoverage, gbc_txtfldRequirementCoverage);

        lblTestingToolType = new JXLabel();
        lblTestingToolType.setText("Testing tool type");
        lblTestingToolType.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblTestingToolType.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblTestingToolType = new GridBagConstraints();
        gbc_lblTestingToolType.anchor = GridBagConstraints.WEST;
        gbc_lblTestingToolType.insets = new Insets(0, 0, 5, 5);
        gbc_lblTestingToolType.gridx = 0;
        gbc_lblTestingToolType.gridy = 5;
        panel_1.add(lblTestingToolType, gbc_lblTestingToolType);

        txtfldTestingToolType = new JXTextField();
        txtfldTestingToolType.setOpaque(false);
        txtfldTestingToolType.setEditable(false);
        txtfldTestingToolType.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldTestingToolType.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldTestingToolType = new GridBagConstraints();
        gbc_txtfldTestingToolType.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldTestingToolType.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldTestingToolType.gridx = 1;
        gbc_txtfldTestingToolType.gridy = 5;
        panel_1.add(txtfldTestingToolType, gbc_txtfldTestingToolType);

        lblRunSet = new JXLabel();
        lblRunSet.setText("Run set");
        lblRunSet.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblRunSet.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblRunSet = new GridBagConstraints();
        gbc_lblRunSet.anchor = GridBagConstraints.WEST;
        gbc_lblRunSet.insets = new Insets(0, 0, 5, 5);
        gbc_lblRunSet.gridx = 2;
        gbc_lblRunSet.gridy = 5;
        panel_1.add(lblRunSet, gbc_lblRunSet);

        txtfldRunSet = new JXTextField();
        txtfldRunSet.setOpaque(false);
        txtfldRunSet.setEditable(false);
        txtfldRunSet.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldRunSet.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldRunSet = new GridBagConstraints();
        gbc_txtfldRunSet.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldRunSet.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldRunSet.gridx = 3;
        gbc_txtfldRunSet.gridy = 5;
        panel_1.add(txtfldRunSet, gbc_txtfldRunSet);

        lblLastRuns = new JXLabel();
        lblLastRuns.setText("Last runs");
        lblLastRuns.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblLastRuns.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblLastRuns = new GridBagConstraints();
        gbc_lblLastRuns.anchor = GridBagConstraints.WEST;
        gbc_lblLastRuns.insets = new Insets(0, 0, 5, 5);
        gbc_lblLastRuns.gridx = 0;
        gbc_lblLastRuns.gridy = 6;
        panel_1.add(lblLastRuns, gbc_lblLastRuns);

        txtfldLastRuns = new JXTextField();
        txtfldLastRuns.setOpaque(false);
        txtfldLastRuns.setEditable(false);
        txtfldLastRuns.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldLastRuns.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldLastRuns = new GridBagConstraints();
        gbc_txtfldLastRuns.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldLastRuns.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldLastRuns.gridx = 1;
        gbc_txtfldLastRuns.gridy = 6;
        panel_1.add(txtfldLastRuns, gbc_txtfldLastRuns);

        lblContent = new JXLabel();
        lblContent.setText("Content");
        lblContent.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblContent.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblContent = new GridBagConstraints();
        gbc_lblContent.anchor = GridBagConstraints.WEST;
        gbc_lblContent.insets = new Insets(0, 0, 5, 5);
        gbc_lblContent.gridx = 2;
        gbc_lblContent.gridy = 6;
        panel_1.add(lblContent, gbc_lblContent);

        txtfldContent = new JXTextField();
        txtfldContent.setOpaque(false);
        txtfldContent.setEditable(false);
        txtfldContent.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldContent.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldContent = new GridBagConstraints();
        gbc_txtfldContent.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldContent.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldContent.gridx = 3;
        gbc_txtfldContent.gridy = 6;
        panel_1.add(txtfldContent, gbc_txtfldContent);

        lblPhase = new JXLabel();
        lblPhase.setText("Phase");
        lblPhase.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblPhase.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblPhase = new GridBagConstraints();
        gbc_lblPhase.anchor = GridBagConstraints.WEST;
        gbc_lblPhase.insets = new Insets(0, 0, 0, 5);
        gbc_lblPhase.gridx = 0;
        gbc_lblPhase.gridy = 7;
        panel_1.add(lblPhase, gbc_lblPhase);

        comboBoxPhase = new JComboBox();
        comboBoxPhase.setEditable(true);
        comboBoxPhase.setBorder(new EmptyBorder(0, 0, 0, 0));
        GridBagConstraints gbc_comboBoxPhase = new GridBagConstraints();
        gbc_comboBoxPhase.fill = GridBagConstraints.HORIZONTAL;
        gbc_comboBoxPhase.insets = new Insets(0, 0, 0, 5);
        gbc_comboBoxPhase.gridx = 1;
        gbc_comboBoxPhase.gridy = 7;
        panel_1.add(comboBoxPhase, gbc_comboBoxPhase);

        lblCoveredContent = new JXLabel();
        lblCoveredContent.setText("Covered Content");
        lblCoveredContent.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblCoveredContent.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblCoveredContent = new GridBagConstraints();
        gbc_lblCoveredContent.anchor = GridBagConstraints.WEST;
        gbc_lblCoveredContent.insets = new Insets(0, 0, 0, 5);
        gbc_lblCoveredContent.gridx = 2;
        gbc_lblCoveredContent.gridy = 7;
        panel_1.add(lblCoveredContent, gbc_lblCoveredContent);

        txtfldCoveredContent = new JXTextField();
        txtfldCoveredContent.setOpaque(false);
        txtfldCoveredContent.setEditable(false);
        txtfldCoveredContent.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldCoveredContent.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldCoveredContent = new GridBagConstraints();
        gbc_txtfldCoveredContent.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldCoveredContent.gridx = 3;
        gbc_txtfldCoveredContent.gridy = 7;
        panel_1.add(txtfldCoveredContent, gbc_txtfldCoveredContent);
    }


    public void setTxtfldAppModules(String txtfldAppModules) {
        this.txtfldAppModules.setText(txtfldAppModules);
    }


    public void setTextRunInReleases(String textRunInReleases) {
        this.textRunInReleases.setText(textRunInReleases);
    }


    public void setTxtfldTestType(String txtfldTestType) {
        this.txtfldTestType.setText(txtfldTestType);
    }


    public void setTxtfldTestingToolType(String txtfldTestingToolType) {
        this.txtfldTestingToolType.setText(txtfldTestingToolType);
    }


    public void setTxtfldLastRuns(String txtfldLastRuns) {
        this.txtfldLastRuns.setText(txtfldLastRuns);
    }


    public void setTxtfldCreated(String txtfldCreated) {
        this.txtfldCreated.setText(txtfldCreated);
    }


    public void setTxtfldEstimetedDuration(String txtfldEstimetedDuration) {
        this.txtfldEstimetedDuration.setText(txtfldEstimetedDuration);
    }


    public void setTxtfldRequirementCoverage(String txtfldRequirementCoverage) {
        this.txtfldRequirementCoverage.setText(txtfldRequirementCoverage);
    }


    public void setTxtfldRunSet(String txtfldRunSet) {
        this.txtfldRunSet.setText(txtfldRunSet);
    }


    public void setTxtfldContent(String txtfldContent) {
        this.txtfldContent.setText(txtfldContent);
    }


    public void setTxtFldCoveredContent(String txtFldCoveredContent) {
        this.txtfldCoveredContent.setText(txtFldCoveredContent);
    }


    public void setTxtfldDescription(String txtfldDescription) {
        this.txtfldDescription.setText(txtfldDescription);
    }

    public void setTxtfldFeature(String txtfldFeature) {
        this.txtfldFeature.setText(txtfldFeature);
    }

    public void setTtxtfldDesigner(String txtfldDesigner) {
        this.txtfldDesigner.setText(txtfldDesigner);
    }

    public void setTxtfldOwner(String txtfldOwner) {
        this.txtfldOwner.setText(txtfldOwner);
    }

    public void setTxtfldManual(String isManual) {
        this.txtfldManual.setText(isManual);
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

    public void setTestIcon(Entity entity) {
        switch (entity) {
            case GHERKIN_TEST:
                this.lblName.setIcon(new ImageIcon(TaskDetailsPanel.class.getResource("/images/gerkinTestIcon.png")));
                break;
            case MANUAL_TEST:
                this.lblName.setIcon(new ImageIcon(TaskDetailsPanel.class.getResource("/images/manualTestIcon.png")));
                break;
        }
    }

}
