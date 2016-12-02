package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;

import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextArea;
import org.jdesktop.swingx.JXTextField;

public class TestDetailsPanel extends JPanel {

	private JXPanel detailsPanel;

	private JXLabel lblDescription;
	private JXTextArea txtfldDescription;

	private JXLabel lblFeature;
	private JXTextField txtfldFeature;

	private JXLabel lblPhase;
	private JComboBox comboBoxPhase;
	private JXLabel lblDesigner;
	private JXTextField txtfldDesigner;
	private JXLabel lblOwner;
	private JXTextField txtfldOwner;
	private JXLabel lblManual;
	private JXTextField txtfldManual;
	private JXLabel lblAppModules;
	private JXTextField txtfldAppModules;
	private JXLabel lblRunInReleases;
	private JXTextField textRunInReleases;
	private JXLabel lblEstimatedDuration;
	private JXLabel lblTestType;
	private JXLabel lblRequirementCoverage;
	private JXLabel lblRunSet;
	private JXLabel lblTestingToolType;
	private JXLabel lblLastRuns;
	private JXLabel lblContent;
	private JXLabel lblCoveredContent;
	private JXLabel lblCreated;
	private JXTextField txtfldTestType;
	private JXTextField txtfldTestingToolType;
	private JXTextField txtfldLastRuns;
	private JXTextField txtfldCreated;
	private JXTextField txtfldEstimetedDuration;
	private JXTextField txtfldRequirementCoverage;
	private JXTextField txtfldRunSet;
	private JXTextField txtfldContent;
	private JXTextField txtFldCoveredContent;
	private JXLabel lblName;

	public TestDetailsPanel() {
		setBounds(100, 100, 918, 291);
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[] { 0, 0 };
		gridBagLayout.rowHeights = new int[] { 0, 0 };
		gridBagLayout.columnWeights = new double[] { 1.0, Double.MIN_VALUE };
		gridBagLayout.rowWeights = new double[] { 1.0, Double.MIN_VALUE };
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
		gbl_parentPanel.columnWidths = new int[] { 0, 0 };
		gbl_parentPanel.rowHeights = new int[] { 0, 0, 0, 0, 0 };
		gbl_parentPanel.columnWeights = new double[] { 1.0, Double.MIN_VALUE };
		gbl_parentPanel.rowWeights = new double[] { 0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE };
		parentPanel.setLayout(gbl_parentPanel);
		
		lblName = new JXLabel();
		lblName.setText("Name");
		lblName.setFont(new Font("Tahoma", Font.BOLD | Font.ITALIC, 13));
		lblName.setBorder(new EmptyBorder(5, 10, 0, 10));
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
		gbl_detailsPanel.columnWidths = new int[] { 0, 0, 0, 0, 0 };
		gbl_detailsPanel.rowHeights = new int[] { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
		gbl_detailsPanel.columnWeights = new double[] { 0.0, 1.0, 0.0, 1.0, Double.MIN_VALUE };
		gbl_detailsPanel.rowWeights = new double[] { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE };
		detailsPanel.setLayout(gbl_detailsPanel);

		lblAppModules = new JXLabel();
		lblAppModules.setText("Application Module");
		lblAppModules.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblAppModules.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblAppModules = new GridBagConstraints();
		gbc_lblAppModules.anchor = GridBagConstraints.WEST;
		gbc_lblAppModules.insets = new Insets(0, 0, 5, 5);
		gbc_lblAppModules.gridx = 0;
		gbc_lblAppModules.gridy = 0;
		detailsPanel.add(lblAppModules, gbc_lblAppModules);

		txtfldAppModules = new JXTextField();
		txtfldAppModules.setOpaque(false);
		txtfldAppModules.setBackground(new Color(0, 0, 0, 0));
		txtfldAppModules.setEditable(false);
		txtfldAppModules.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		txtfldAppModules.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldAppModules = new GridBagConstraints();
		gbc_txtfldAppModules.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldAppModules.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldAppModules.gridx = 1;
		gbc_txtfldAppModules.gridy = 0;
		detailsPanel.add(txtfldAppModules, gbc_txtfldAppModules);

		lblCreated = new JXLabel();
		lblCreated.setText("Created");
		lblCreated.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblCreated.setBorder(new EmptyBorder(0, 10, 0, 10));
		GridBagConstraints gbc_lblCreated = new GridBagConstraints();
		gbc_lblCreated.anchor = GridBagConstraints.WEST;
		gbc_lblCreated.insets = new Insets(0, 0, 5, 5);
		gbc_lblCreated.gridx = 2;
		gbc_lblCreated.gridy = 0;
		detailsPanel.add(lblCreated, gbc_lblCreated);

		txtfldCreated = new JXTextField();
		txtfldCreated.setEditable(false);
		txtfldCreated.setOpaque(false);
		txtfldCreated.setBackground(new Color(0, 0, 0, 0));
		txtfldCreated.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtfldCreated = new GridBagConstraints();
		gbc_txtfldCreated.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldCreated.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldCreated.gridx = 3;
		gbc_txtfldCreated.gridy = 0;
		detailsPanel.add(txtfldCreated, gbc_txtfldCreated);

		lblDesigner = new JXLabel();
		lblDesigner.setText("Designer");
		lblDesigner.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblDesigner.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblDesigner = new GridBagConstraints();
		gbc_lblDesigner.anchor = GridBagConstraints.WEST;
		gbc_lblDesigner.insets = new Insets(0, 0, 5, 5);
		gbc_lblDesigner.gridx = 0;
		gbc_lblDesigner.gridy = 1;
		detailsPanel.add(lblDesigner, gbc_lblDesigner);

		txtfldDesigner = new JXTextField();
		txtfldDesigner.setEditable(false);
		txtfldDesigner.setOpaque(false);
		txtfldDesigner.setBackground(new Color(0, 0, 0, 0));
		txtfldDesigner.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtfldDesigner = new GridBagConstraints();
		gbc_txtfldDesigner.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldDesigner.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldDesigner.gridx = 1;
		gbc_txtfldDesigner.gridy = 1;
		detailsPanel.add(txtfldDesigner, gbc_txtfldDesigner);

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

		lblManual = new JXLabel();
		lblManual.setBorder(new EmptyBorder(0, 0, 0, 10));
		lblManual.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblManual.setText("Manual");
		GridBagConstraints gbc_lblManual = new GridBagConstraints();
		gbc_lblManual.anchor = GridBagConstraints.WEST;
		gbc_lblManual.insets = new Insets(0, 0, 5, 5);
		gbc_lblManual.gridx = 0;
		gbc_lblManual.gridy = 2;
		detailsPanel.add(lblManual, gbc_lblManual);

		txtfldManual = new JXTextField();
		txtfldManual.setEditable(false);
		txtfldManual.setOpaque(false);
		txtfldManual.setBackground(new Color(0, 0, 0, 0));
		txtfldManual.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtfldManual = new GridBagConstraints();
		gbc_txtfldManual.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldManual.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldManual.gridx = 1;
		gbc_txtfldManual.gridy = 2;
		detailsPanel.add(txtfldManual, gbc_txtfldManual);

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

		lblRunInReleases = new JXLabel();
		lblRunInReleases.setText("Run in Releases");
		lblRunInReleases.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblRunInReleases.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblRunInReleases = new GridBagConstraints();
		gbc_lblRunInReleases.anchor = GridBagConstraints.WEST;
		gbc_lblRunInReleases.insets = new Insets(0, 0, 5, 5);
		gbc_lblRunInReleases.gridx = 0;
		gbc_lblRunInReleases.gridy = 3;
		detailsPanel.add(lblRunInReleases, gbc_lblRunInReleases);

		textRunInReleases = new JXTextField();
		textRunInReleases.setEditable(false);
		textRunInReleases.setOpaque(false);
		textRunInReleases.setBackground(new Color(0, 0, 0, 0));
		textRunInReleases.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_textRunInReleases = new GridBagConstraints();
		gbc_textRunInReleases.insets = new Insets(0, 0, 5, 5);
		gbc_textRunInReleases.fill = GridBagConstraints.HORIZONTAL;
		gbc_textRunInReleases.gridx = 1;
		gbc_textRunInReleases.gridy = 3;
		detailsPanel.add(textRunInReleases, gbc_textRunInReleases);

		lblEstimatedDuration = new JXLabel();
		lblEstimatedDuration.setText("Estimated duration");
		lblEstimatedDuration.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblEstimatedDuration.setBorder(new EmptyBorder(0, 10, 0, 10));
		GridBagConstraints gbc_lblEstimatedDuration = new GridBagConstraints();
		gbc_lblEstimatedDuration.anchor = GridBagConstraints.WEST;
		gbc_lblEstimatedDuration.insets = new Insets(0, 0, 5, 5);
		gbc_lblEstimatedDuration.gridx = 2;
		gbc_lblEstimatedDuration.gridy = 3;
		detailsPanel.add(lblEstimatedDuration, gbc_lblEstimatedDuration);

		txtfldEstimetedDuration = new JXTextField();
		txtfldEstimetedDuration.setEditable(false);
		txtfldEstimetedDuration.setOpaque(false);
		txtfldEstimetedDuration.setBackground(new Color(0, 0, 0, 0));
		txtfldEstimetedDuration.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtfldEstimetedDuration = new GridBagConstraints();
		gbc_txtfldEstimetedDuration.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldEstimetedDuration.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldEstimetedDuration.gridx = 3;
		gbc_txtfldEstimetedDuration.gridy = 3;
		detailsPanel.add(txtfldEstimetedDuration, gbc_txtfldEstimetedDuration);

		lblTestType = new JXLabel();
		lblTestType.setText("Test type");
		lblTestType.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblTestType.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblTestType = new GridBagConstraints();
		gbc_lblTestType.anchor = GridBagConstraints.WEST;
		gbc_lblTestType.insets = new Insets(0, 0, 5, 5);
		gbc_lblTestType.gridx = 0;
		gbc_lblTestType.gridy = 4;
		detailsPanel.add(lblTestType, gbc_lblTestType);

		txtfldTestType = new JXTextField();
		txtfldTestType.setEditable(false);
		txtfldTestType.setOpaque(false);
		txtfldTestType.setBackground(new Color(0, 0, 0, 0));
		txtfldTestType.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtfldTestType = new GridBagConstraints();
		gbc_txtfldTestType.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldTestType.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldTestType.gridx = 1;
		gbc_txtfldTestType.gridy = 4;
		detailsPanel.add(txtfldTestType, gbc_txtfldTestType);

		lblRequirementCoverage = new JXLabel();
		lblRequirementCoverage.setText("Requirement coverage");
		lblRequirementCoverage.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblRequirementCoverage.setBorder(new EmptyBorder(0, 10, 0, 10));
		GridBagConstraints gbc_lblRequirementCoverage = new GridBagConstraints();
		gbc_lblRequirementCoverage.anchor = GridBagConstraints.WEST;
		gbc_lblRequirementCoverage.insets = new Insets(0, 0, 5, 5);
		gbc_lblRequirementCoverage.gridx = 2;
		gbc_lblRequirementCoverage.gridy = 4;
		detailsPanel.add(lblRequirementCoverage, gbc_lblRequirementCoverage);

		txtfldRequirementCoverage = new JXTextField();
		txtfldRequirementCoverage.setEditable(false);
		txtfldRequirementCoverage.setOpaque(false);
		txtfldRequirementCoverage.setBackground(new Color(0, 0, 0, 0));
		txtfldRequirementCoverage.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtfldRequirementCoverage = new GridBagConstraints();
		gbc_txtfldRequirementCoverage.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldRequirementCoverage.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldRequirementCoverage.gridx = 3;
		gbc_txtfldRequirementCoverage.gridy = 4;
		detailsPanel.add(txtfldRequirementCoverage, gbc_txtfldRequirementCoverage);

		lblTestingToolType = new JXLabel();
		lblTestingToolType.setText("Testing tool type");
		lblTestingToolType.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblTestingToolType.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblTestingToolType = new GridBagConstraints();
		gbc_lblTestingToolType.anchor = GridBagConstraints.WEST;
		gbc_lblTestingToolType.insets = new Insets(0, 0, 5, 5);
		gbc_lblTestingToolType.gridx = 0;
		gbc_lblTestingToolType.gridy = 5;
		detailsPanel.add(lblTestingToolType, gbc_lblTestingToolType);

		txtfldTestingToolType = new JXTextField();
		txtfldTestingToolType.setEditable(false);
		txtfldTestingToolType.setOpaque(false);
		txtfldTestingToolType.setBackground(new Color(0, 0, 0, 0));
		txtfldTestingToolType.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtfldTestingToolType = new GridBagConstraints();
		gbc_txtfldTestingToolType.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldTestingToolType.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldTestingToolType.gridx = 1;
		gbc_txtfldTestingToolType.gridy = 5;
		detailsPanel.add(txtfldTestingToolType, gbc_txtfldTestingToolType);

		lblRunSet = new JXLabel();
		lblRunSet.setText("Run set");
		lblRunSet.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblRunSet.setBorder(new EmptyBorder(0, 10, 0, 10));
		GridBagConstraints gbc_lblRunSet = new GridBagConstraints();
		gbc_lblRunSet.anchor = GridBagConstraints.WEST;
		gbc_lblRunSet.insets = new Insets(0, 0, 5, 5);
		gbc_lblRunSet.gridx = 2;
		gbc_lblRunSet.gridy = 5;
		detailsPanel.add(lblRunSet, gbc_lblRunSet);

		txtfldRunSet = new JXTextField();
		txtfldRunSet.setEditable(false);
		txtfldRunSet.setOpaque(false);
		txtfldRunSet.setBackground(new Color(0, 0, 0, 0));
		txtfldRunSet.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtfldRunSet = new GridBagConstraints();
		gbc_txtfldRunSet.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldRunSet.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldRunSet.gridx = 3;
		gbc_txtfldRunSet.gridy = 5;
		detailsPanel.add(txtfldRunSet, gbc_txtfldRunSet);

		lblLastRuns = new JXLabel();
		lblLastRuns.setText("Last runs");
		lblLastRuns.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblLastRuns.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblLastRuns = new GridBagConstraints();
		gbc_lblLastRuns.anchor = GridBagConstraints.WEST;
		gbc_lblLastRuns.insets = new Insets(0, 0, 5, 5);
		gbc_lblLastRuns.gridx = 0;
		gbc_lblLastRuns.gridy = 6;
		detailsPanel.add(lblLastRuns, gbc_lblLastRuns);

		txtfldLastRuns = new JXTextField();
		txtfldLastRuns.setEditable(false);
		txtfldLastRuns.setOpaque(false);
		txtfldLastRuns.setBackground(new Color(0, 0, 0, 0));
		txtfldLastRuns.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtfldLastRuns = new GridBagConstraints();
		gbc_txtfldLastRuns.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldLastRuns.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldLastRuns.gridx = 1;
		gbc_txtfldLastRuns.gridy = 6;
		detailsPanel.add(txtfldLastRuns, gbc_txtfldLastRuns);

		lblContent = new JXLabel();
		lblContent.setText("Content");
		lblContent.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblContent.setBorder(new EmptyBorder(0, 10, 0, 10));
		GridBagConstraints gbc_lblContent = new GridBagConstraints();
		gbc_lblContent.anchor = GridBagConstraints.WEST;
		gbc_lblContent.insets = new Insets(0, 0, 5, 5);
		gbc_lblContent.gridx = 2;
		gbc_lblContent.gridy = 6;
		detailsPanel.add(lblContent, gbc_lblContent);

		txtfldContent = new JXTextField();
		txtfldContent.setEditable(false);
		txtfldContent.setOpaque(false);
		txtfldContent.setBackground(new Color(0, 0, 0, 0));
		txtfldContent.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtfldContent = new GridBagConstraints();
		gbc_txtfldContent.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldContent.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldContent.gridx = 3;
		gbc_txtfldContent.gridy = 6;
		detailsPanel.add(txtfldContent, gbc_txtfldContent);

		lblPhase = new JXLabel();
		lblPhase.setBorder(new EmptyBorder(0, 0, 0, 10));
		lblPhase.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblPhase.setText("Phase");
		GridBagConstraints gbc_lblPhase = new GridBagConstraints();
		gbc_lblPhase.anchor = GridBagConstraints.WEST;
		gbc_lblPhase.insets = new Insets(0, 0, 0, 5);
		gbc_lblPhase.gridx = 0;
		gbc_lblPhase.gridy = 7;
		detailsPanel.add(lblPhase, gbc_lblPhase);

		comboBoxPhase = new JComboBox();
		comboBoxPhase.setEditable(true);
		comboBoxPhase.setBorder(new EmptyBorder(0, 0, 0, 0));
		comboBoxPhase.setModel(new DefaultComboBoxModel(new String[] { "Open", "In Progress", "Done" }));
		GridBagConstraints gbc_comboBox = new GridBagConstraints();
		gbc_comboBox.insets = new Insets(0, 0, 0, 5);
		gbc_comboBox.fill = GridBagConstraints.HORIZONTAL;
		gbc_comboBox.gridx = 1;
		gbc_comboBox.gridy = 7;
		detailsPanel.add(comboBoxPhase, gbc_comboBox);

		lblCoveredContent = new JXLabel();
		lblCoveredContent.setText("Covered Content");
		lblCoveredContent.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblCoveredContent.setBorder(new EmptyBorder(0, 10, 0, 10));
		GridBagConstraints gbc_lblCoveredContent = new GridBagConstraints();
		gbc_lblCoveredContent.anchor = GridBagConstraints.WEST;
		gbc_lblCoveredContent.insets = new Insets(0, 0, 0, 5);
		gbc_lblCoveredContent.gridx = 2;
		gbc_lblCoveredContent.gridy = 7;
		detailsPanel.add(lblCoveredContent, gbc_lblCoveredContent);

		txtFldCoveredContent = new JXTextField();
		txtFldCoveredContent.setEditable(false);
		txtFldCoveredContent.setOpaque(false);
		txtFldCoveredContent.setBackground(new Color(0, 0, 0, 0));
		txtFldCoveredContent.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
		GridBagConstraints gbc_txtFldCoveredContent = new GridBagConstraints();
		gbc_txtFldCoveredContent.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtFldCoveredContent.gridx = 3;
		gbc_txtFldCoveredContent.gridy = 7;
		detailsPanel.add(txtFldCoveredContent, gbc_txtFldCoveredContent);
	}

	
	public void setTxtfldAppModules(String txtfldAppModules) {
		this.txtfldAppModules.setText(txtfldAppModules);;
	}


	public void setTextRunInReleases(String textRunInReleases) {
		this.textRunInReleases.setText(textRunInReleases);;
	}


	public void setTxtfldTestType(String txtfldTestType) {
		this.txtfldTestType.setText(txtfldTestType);;
	}


	public void setTxtfldTestingToolType(String txtfldTestingToolType) {
		this.txtfldTestingToolType.setText(txtfldTestingToolType);;
	}


	public void setTxtfldLastRuns(String txtfldLastRuns) {
		this.txtfldLastRuns.setText(txtfldLastRuns);;
	}


	public void setTxtfldCreated(String txtfldCreated) {
		this.txtfldCreated.setText(txtfldCreated);;
	}


	public void setTxtfldEstimetedDuration(String txtfldEstimetedDuration) {
		this.txtfldEstimetedDuration.setText(txtfldEstimetedDuration);;
	}


	public void setTxtfldRequirementCoverage(String txtfldRequirementCoverage) {
		this.txtfldRequirementCoverage.setText(txtfldRequirementCoverage);;
	}


	public void setTxtfldRunSet(String txtfldRunSet) {
		this.txtfldRunSet.setText(txtfldRunSet);;
	}


	public void setTxtfldContent(String txtfldContent) {
		this.txtfldContent.setText(txtfldContent);;
	}


	public void setTxtFldCoveredContent(String txtFldCoveredContent) {
		this.txtFldCoveredContent.setText(txtFldCoveredContent);;
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
		this.lblName.setText("Name: "+lblName);
	}

}
