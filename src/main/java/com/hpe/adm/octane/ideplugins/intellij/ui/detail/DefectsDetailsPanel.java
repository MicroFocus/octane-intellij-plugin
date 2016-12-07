package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextArea;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;

public class DefectsDetailsPanel extends JPanel {
	private JXPanel rootPanel;
	private JXLabel lblDescription;
	private JXTextArea txtfldDescription;
	private JXPanel defectsDetailsPanel;
	private JXLabel lblFeature;
	private JXTextField txtfldFeature;
	private JXLabel lblDetectedBy;
	private JXTextField txtfldDetectedBy;
	private JXLabel lblQaOwner;
	private JXTextField txtfldQaOwner;
	private JXLabel lblBlocked;
	private JXTextField txtfldBlocked;
	private JXLabel lblSeverity;
	private JXTextField txtfldSeverity;
	private JXLabel lblBlockedReason;
	private JXTextField txtfldBlockedReason;
	private JXLabel label_8;
	private JXTextField txtfldTeam;
	private JXLabel lblGroup;
	private JXTextField txtfldGroup;
	private JXLabel lblStoryPoints;
	private JXTextField txtfldStoryPoints;
	private JXLabel lblFeedbackType;
	private JXTextField txtfldFeedbackType;
	private JXLabel lblPriority;
	private JXTextField txtfldPriority;
	private JXLabel lblEnviroment;
	private JXTextField txtfldEnviroment;
	private JXLabel lblPhase;
	private JComboBox comboBoxPhase;
	private JXLabel lblAppModules;
	private JXTextField txtfldAppModules;
	private JXTextField textField_13;
	private JXTextField textField_14;
	private JXLabel lblRelease;
	private JXTextField txtfldRelease;
	private JXLabel lblDetectedInBuild;
	private JXTextField txtfldDetectedInBuild;
	private JXLabel lblSprint;
	private JXTextField txtfldSprint;
	private JXLabel lblDetectedInRelease;
	private JXTextField txtfldDetectedInRelease;
	private JXLabel lblFixedInBuild;
	private JXTextField txtfldFixedInBuild;
	private JXPanel atachementsPanel;
	private JXLabel lblAttachments;
	private JXLabel listOfAttachments;
	private JXPanel nameAndIconPanel;
	private JXLabel lblName;

	public DefectsDetailsPanel() {
		setBorder(null);
		setBounds(100, 100, 900, 430);
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[] { 0, 0 };
		gridBagLayout.rowHeights = new int[] { 0, 0 };
		gridBagLayout.columnWeights = new double[] { 1.0, Double.MIN_VALUE };
		gridBagLayout.rowWeights = new double[] { 1.0, Double.MIN_VALUE };
		this.setLayout(gridBagLayout);

		rootPanel = new JXPanel();
//		rootPanel.setPreferredSize(new Dimension(900, 450));
		rootPanel.setBorder(new EmptyBorder(10, 30, 30, 30));
		GridBagConstraints gbc_rootPanel = new GridBagConstraints();
		gbc_rootPanel.fill = GridBagConstraints.BOTH;
		gbc_rootPanel.gridx = 0;
		gbc_rootPanel.gridy = 0;
		add(rootPanel, gbc_rootPanel);
		GridBagLayout gbl_rootPanel = new GridBagLayout();
		gbl_rootPanel.columnWidths = new int[]{0, 0};
		gbl_rootPanel.rowHeights = new int[]{0, 0, 0, 0, 0, 0};
		gbl_rootPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_rootPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
		rootPanel.setLayout(gbl_rootPanel);

		nameAndIconPanel = new JXPanel();
		nameAndIconPanel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		FlowLayout fl_nameAndIconPanel = (FlowLayout) nameAndIconPanel.getLayout();
		fl_nameAndIconPanel.setHgap(0);
		fl_nameAndIconPanel.setAlignment(FlowLayout.LEFT);
		GridBagConstraints gbc_nameAndIconPanel = new GridBagConstraints();
		gbc_nameAndIconPanel.insets = new Insets(0, 0, 5, 0);
		gbc_nameAndIconPanel.fill = GridBagConstraints.BOTH;
		gbc_nameAndIconPanel.gridx = 0;
		gbc_nameAndIconPanel.gridy = 0;
		rootPanel.add(nameAndIconPanel, gbc_nameAndIconPanel);
		
		lblName = new JXLabel();
		lblName.setIcon(new ImageIcon(DefectsDetailsPanel.class.getResource("/images/defectIcon.png")));
		lblName.setText("Name");
		lblName.setFont(new Font("Tahoma", Font.BOLD, 12));
		lblName.setBorder(null);
		nameAndIconPanel.add(lblName);
		
		lblDescription = new JXLabel();
		lblDescription.setText("Description");
		lblDescription.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblDescription.setBorder(null);
		GridBagConstraints gbc_lblDescription = new GridBagConstraints();
		gbc_lblDescription.anchor = GridBagConstraints.NORTHWEST;
		gbc_lblDescription.insets = new Insets(0, 0, 5, 0);
		gbc_lblDescription.gridx = 0;
		gbc_lblDescription.gridy = 1;
		rootPanel.add(lblDescription, gbc_lblDescription);
		
		txtfldDescription = new JXTextArea();
		txtfldDescription.setOpaque(false);
		txtfldDescription.setLineWrap(true);
		txtfldDescription.setEditable(false);
		txtfldDescription.setBorder(null);
		txtfldDescription.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldDescription = new GridBagConstraints();
		gbc_txtfldDescription.fill = GridBagConstraints.BOTH;
		gbc_txtfldDescription.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldDescription.gridx = 0;
		gbc_txtfldDescription.gridy = 2;
		rootPanel.add(txtfldDescription, gbc_txtfldDescription);

		defectsDetailsPanel = new JXPanel();
		defectsDetailsPanel.setOpaque(false);
		defectsDetailsPanel.setBorder(new EmptyBorder(10, 0, 15, 0));
		GridBagConstraints gbc_defectsDetailsPanel = new GridBagConstraints();
		gbc_defectsDetailsPanel.fill = GridBagConstraints.BOTH;
		gbc_defectsDetailsPanel.insets = new Insets(0, 0, 5, 0);
		gbc_defectsDetailsPanel.gridx = 0;
		gbc_defectsDetailsPanel.gridy = 3;
		rootPanel.add(defectsDetailsPanel, gbc_defectsDetailsPanel);
		GridBagLayout gbl_defectsDetailsPanel = new GridBagLayout();
		gbl_defectsDetailsPanel.columnWidths = new int[]{0, 0, 0, 0, 0};
		gbl_defectsDetailsPanel.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
		gbl_defectsDetailsPanel.columnWeights = new double[]{0.0, 1.0, 0.0, 1.0, Double.MIN_VALUE};
		gbl_defectsDetailsPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		defectsDetailsPanel.setLayout(gbl_defectsDetailsPanel);
		
		lblFeature = new JXLabel();
		lblFeature.setText("Feature");
		lblFeature.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblFeature.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblFeature = new GridBagConstraints();
		gbc_lblFeature.anchor = GridBagConstraints.WEST;
		gbc_lblFeature.insets = new Insets(0, 0, 5, 5);
		gbc_lblFeature.gridx = 0;
		gbc_lblFeature.gridy = 0;
		defectsDetailsPanel.add(lblFeature, gbc_lblFeature);
		
		txtfldFeature = new JXTextField();
		txtfldFeature.setOpaque(false);
		txtfldFeature.setEditable(false);
		txtfldFeature.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldFeature.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldFeature = new GridBagConstraints();
		gbc_txtfldFeature.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldFeature.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldFeature.gridx = 1;
		gbc_txtfldFeature.gridy = 0;
		defectsDetailsPanel.add(txtfldFeature, gbc_txtfldFeature);
		
		lblDetectedBy = new JXLabel();
		lblDetectedBy.setText("Detected by");
		lblDetectedBy.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblDetectedBy.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lblDetectedBy = new GridBagConstraints();
		gbc_lblDetectedBy.anchor = GridBagConstraints.SOUTHWEST;
		gbc_lblDetectedBy.insets = new Insets(0, 0, 5, 5);
		gbc_lblDetectedBy.gridx = 2;
		gbc_lblDetectedBy.gridy = 0;
		defectsDetailsPanel.add(lblDetectedBy, gbc_lblDetectedBy);
		
		txtfldDetectedBy = new JXTextField();
		txtfldDetectedBy.setOpaque(false);
		txtfldDetectedBy.setEditable(false);
		txtfldDetectedBy.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldDetectedBy.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldDetectedBy = new GridBagConstraints();
		gbc_txtfldDetectedBy.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldDetectedBy.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldDetectedBy.gridx = 3;
		gbc_txtfldDetectedBy.gridy = 0;
		defectsDetailsPanel.add(txtfldDetectedBy, gbc_txtfldDetectedBy);
		
		lblQaOwner = new JXLabel();
		lblQaOwner.setText("QA Owner");
		lblQaOwner.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblQaOwner.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblQaOwner = new GridBagConstraints();
		gbc_lblQaOwner.anchor = GridBagConstraints.WEST;
		gbc_lblQaOwner.insets = new Insets(0, 0, 5, 5);
		gbc_lblQaOwner.gridx = 0;
		gbc_lblQaOwner.gridy = 1;
		defectsDetailsPanel.add(lblQaOwner, gbc_lblQaOwner);
		
		txtfldQaOwner = new JXTextField();
		txtfldQaOwner.setOpaque(false);
		txtfldQaOwner.setEditable(false);
		txtfldQaOwner.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldQaOwner.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldQaOwner = new GridBagConstraints();
		gbc_txtfldQaOwner.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldQaOwner.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldQaOwner.gridx = 1;
		gbc_txtfldQaOwner.gridy = 1;
		defectsDetailsPanel.add(txtfldQaOwner, gbc_txtfldQaOwner);
		
		lblBlocked = new JXLabel();
		lblBlocked.setText("Blocked");
		lblBlocked.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblBlocked.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lblBlocked = new GridBagConstraints();
		gbc_lblBlocked.anchor = GridBagConstraints.SOUTHWEST;
		gbc_lblBlocked.insets = new Insets(0, 0, 5, 5);
		gbc_lblBlocked.gridx = 2;
		gbc_lblBlocked.gridy = 1;
		defectsDetailsPanel.add(lblBlocked, gbc_lblBlocked);
		
		txtfldBlocked = new JXTextField();
		txtfldBlocked.setOpaque(false);
		txtfldBlocked.setEditable(false);
		txtfldBlocked.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldBlocked.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldBlocked = new GridBagConstraints();
		gbc_txtfldBlocked.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldBlocked.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldBlocked.gridx = 3;
		gbc_txtfldBlocked.gridy = 1;
		defectsDetailsPanel.add(txtfldBlocked, gbc_txtfldBlocked);
		
		lblSeverity = new JXLabel();
		lblSeverity.setText("Severity");
		lblSeverity.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblSeverity.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblSeverity = new GridBagConstraints();
		gbc_lblSeverity.anchor = GridBagConstraints.SOUTHWEST;
		gbc_lblSeverity.insets = new Insets(0, 0, 5, 5);
		gbc_lblSeverity.gridx = 0;
		gbc_lblSeverity.gridy = 2;
		defectsDetailsPanel.add(lblSeverity, gbc_lblSeverity);
		
		txtfldSeverity = new JXTextField();
		txtfldSeverity.setOpaque(false);
		txtfldSeverity.setEditable(false);
		txtfldSeverity.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldSeverity.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldSeverity = new GridBagConstraints();
		gbc_txtfldSeverity.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldSeverity.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldSeverity.gridx = 1;
		gbc_txtfldSeverity.gridy = 2;
		defectsDetailsPanel.add(txtfldSeverity, gbc_txtfldSeverity);
		
		lblBlockedReason = new JXLabel();
		lblBlockedReason.setText("Blocked Reason");
		lblBlockedReason.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblBlockedReason.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lblBlockedReason = new GridBagConstraints();
		gbc_lblBlockedReason.anchor = GridBagConstraints.SOUTHWEST;
		gbc_lblBlockedReason.insets = new Insets(0, 0, 5, 5);
		gbc_lblBlockedReason.gridx = 2;
		gbc_lblBlockedReason.gridy = 2;
		defectsDetailsPanel.add(lblBlockedReason, gbc_lblBlockedReason);
		
		txtfldBlockedReason = new JXTextField();
		txtfldBlockedReason.setOpaque(false);
		txtfldBlockedReason.setEditable(false);
		txtfldBlockedReason.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldBlockedReason.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldBlockedReason = new GridBagConstraints();
		gbc_txtfldBlockedReason.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldBlockedReason.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldBlockedReason.gridx = 3;
		gbc_txtfldBlockedReason.gridy = 2;
		defectsDetailsPanel.add(txtfldBlockedReason, gbc_txtfldBlockedReason);

		label_8 = new JXLabel();
		label_8.setText("Team");
		label_8.setFont(new Font("Tahoma", Font.BOLD, 11));
		label_8.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_label_8 = new GridBagConstraints();
		gbc_label_8.anchor = GridBagConstraints.WEST;
		gbc_label_8.insets = new Insets(0, 0, 5, 5);
		gbc_label_8.gridx = 0;
		gbc_label_8.gridy = 3;
		defectsDetailsPanel.add(label_8, gbc_label_8);
		
		txtfldTeam = new JXTextField();
		txtfldTeam.setOpaque(false);
		txtfldTeam.setEditable(false);
		txtfldTeam.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldTeam.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldTeam = new GridBagConstraints();
		gbc_txtfldTeam.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldTeam.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldTeam.gridx = 1;
		gbc_txtfldTeam.gridy = 3;
		defectsDetailsPanel.add(txtfldTeam, gbc_txtfldTeam);
		
		lblGroup = new JXLabel();
		lblGroup.setText("Group");
		lblGroup.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblGroup.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lblGroup = new GridBagConstraints();
		gbc_lblGroup.anchor = GridBagConstraints.WEST;
		gbc_lblGroup.insets = new Insets(0, 0, 5, 5);
		gbc_lblGroup.gridx = 2;
		gbc_lblGroup.gridy = 3;
		defectsDetailsPanel.add(lblGroup, gbc_lblGroup);
		
		txtfldGroup = new JXTextField();
		txtfldGroup.setOpaque(false);
		txtfldGroup.setEditable(false);
		txtfldGroup.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldGroup.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldGroup = new GridBagConstraints();
		gbc_txtfldGroup.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldGroup.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldGroup.gridx = 3;
		gbc_txtfldGroup.gridy = 3;
		defectsDetailsPanel.add(txtfldGroup, gbc_txtfldGroup);
		
		lblStoryPoints = new JXLabel();
		lblStoryPoints.setText("Story Points");
		lblStoryPoints.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblStoryPoints.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblStoryPoints = new GridBagConstraints();
		gbc_lblStoryPoints.anchor = GridBagConstraints.WEST;
		gbc_lblStoryPoints.insets = new Insets(0, 0, 5, 5);
		gbc_lblStoryPoints.gridx = 0;
		gbc_lblStoryPoints.gridy = 4;
		defectsDetailsPanel.add(lblStoryPoints, gbc_lblStoryPoints);

		txtfldStoryPoints = new JXTextField();
		txtfldStoryPoints.setOpaque(false);
		txtfldStoryPoints.setEditable(false);
		txtfldStoryPoints.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldStoryPoints.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_textfldStoryPoints = new GridBagConstraints();
		gbc_textfldStoryPoints.fill = GridBagConstraints.HORIZONTAL;
		gbc_textfldStoryPoints.insets = new Insets(0, 0, 5, 5);
		gbc_textfldStoryPoints.gridx = 1;
		gbc_textfldStoryPoints.gridy = 4;
		defectsDetailsPanel.add(txtfldStoryPoints, gbc_textfldStoryPoints);
		
		lblFeedbackType = new JXLabel();
		lblFeedbackType.setText("Feedback type");
		lblFeedbackType.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblFeedbackType.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lblFeedbackType = new GridBagConstraints();
		gbc_lblFeedbackType.anchor = GridBagConstraints.SOUTHWEST;
		gbc_lblFeedbackType.insets = new Insets(0, 0, 5, 5);
		gbc_lblFeedbackType.gridx = 2;
		gbc_lblFeedbackType.gridy = 4;
		defectsDetailsPanel.add(lblFeedbackType, gbc_lblFeedbackType);
		
		txtfldFeedbackType = new JXTextField();
		txtfldFeedbackType.setOpaque(false);
		txtfldFeedbackType.setEditable(false);
		txtfldFeedbackType.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldFeedbackType.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldFeedbackType = new GridBagConstraints();
		gbc_txtfldFeedbackType.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldFeedbackType.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldFeedbackType.gridx = 3;
		gbc_txtfldFeedbackType.gridy = 4;
		defectsDetailsPanel.add(txtfldFeedbackType, gbc_txtfldFeedbackType);
		
		lblPriority = new JXLabel();
		lblPriority.setText("Priority");
		lblPriority.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblPriority.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblPriority = new GridBagConstraints();
		gbc_lblPriority.anchor = GridBagConstraints.WEST;
		gbc_lblPriority.insets = new Insets(0, 0, 5, 5);
		gbc_lblPriority.gridx = 0;
		gbc_lblPriority.gridy = 5;
		defectsDetailsPanel.add(lblPriority, gbc_lblPriority);

		txtfldPriority = new JXTextField();
		txtfldPriority.setOpaque(false);
		txtfldPriority.setEditable(false);
		txtfldPriority.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldPriority.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_textfldPriority = new GridBagConstraints();
		gbc_textfldPriority.fill = GridBagConstraints.HORIZONTAL;
		gbc_textfldPriority.insets = new Insets(0, 0, 5, 5);
		gbc_textfldPriority.gridx = 1;
		gbc_textfldPriority.gridy = 5;
		defectsDetailsPanel.add(txtfldPriority, gbc_textfldPriority);
		
		lblEnviroment = new JXLabel();
		lblEnviroment.setText("Enviroment");
		lblEnviroment.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblEnviroment.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lblEnviroment = new GridBagConstraints();
		gbc_lblEnviroment.anchor = GridBagConstraints.WEST;
		gbc_lblEnviroment.insets = new Insets(0, 0, 5, 5);
		gbc_lblEnviroment.gridx = 2;
		gbc_lblEnviroment.gridy = 5;
		defectsDetailsPanel.add(lblEnviroment, gbc_lblEnviroment);
		
		txtfldEnviroment = new JXTextField();
		txtfldEnviroment.setOpaque(false);
		txtfldEnviroment.setEditable(false);
		txtfldEnviroment.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldEnviroment.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldEnviroment = new GridBagConstraints();
		gbc_txtfldEnviroment.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldEnviroment.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldEnviroment.gridx = 3;
		gbc_txtfldEnviroment.gridy = 5;
		defectsDetailsPanel.add(txtfldEnviroment, gbc_txtfldEnviroment);
		
		lblPhase = new JXLabel();
		lblPhase.setText("Phase");
		lblPhase.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblPhase.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblPhase = new GridBagConstraints();
		gbc_lblPhase.anchor = GridBagConstraints.WEST;
		gbc_lblPhase.insets = new Insets(0, 0, 5, 5);
		gbc_lblPhase.gridx = 0;
		gbc_lblPhase.gridy = 6;
		defectsDetailsPanel.add(lblPhase, gbc_lblPhase);
		
		comboBoxPhase = new JComboBox();
		comboBoxPhase.setEditable(true);
		comboBoxPhase.setBorder(new EmptyBorder(0, 0, 0, 0));
		GridBagConstraints gbc_comboBoxPhase = new GridBagConstraints();
		gbc_comboBoxPhase.fill = GridBagConstraints.HORIZONTAL;
		gbc_comboBoxPhase.insets = new Insets(0, 0, 5, 5);
		gbc_comboBoxPhase.gridx = 1;
		gbc_comboBoxPhase.gridy = 6;
		defectsDetailsPanel.add(comboBoxPhase, gbc_comboBoxPhase);

		lblAppModules = new JXLabel();
		lblAppModules.setText("Application Module");
		lblAppModules.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblAppModules.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lblAppModules = new GridBagConstraints();
		gbc_lblAppModules.anchor = GridBagConstraints.SOUTHEAST;
		gbc_lblAppModules.insets = new Insets(0, 0, 5, 5);
		gbc_lblAppModules.gridx = 2;
		gbc_lblAppModules.gridy = 6;
		defectsDetailsPanel.add(lblAppModules, gbc_lblAppModules);
		
		txtfldAppModules = new JXTextField();
		txtfldAppModules.setOpaque(false);
		txtfldAppModules.setEditable(false);
		txtfldAppModules.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldAppModules.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldAppModules = new GridBagConstraints();
		gbc_txtfldAppModules.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldAppModules.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldAppModules.gridx = 3;
		gbc_txtfldAppModules.gridy = 6;
		defectsDetailsPanel.add(txtfldAppModules, gbc_txtfldAppModules);

		textField_13 = new JXTextField();
		textField_13.setOpaque(false);
		textField_13.setEditable(false);
		textField_13.setBorder(new EmptyBorder(5, 0, 5, 0));
		textField_13.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_textField_13 = new GridBagConstraints();
		gbc_textField_13.fill = GridBagConstraints.HORIZONTAL;
		gbc_textField_13.insets = new Insets(0, 0, 5, 5);
		gbc_textField_13.gridx = 1;
		gbc_textField_13.gridy = 7;
		defectsDetailsPanel.add(textField_13, gbc_textField_13);

		textField_14 = new JXTextField();
		textField_14.setOpaque(false);
		textField_14.setEditable(false);
		textField_14.setBorder(new EmptyBorder(5, 0, 5, 0));
		textField_14.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_textField_14 = new GridBagConstraints();
		gbc_textField_14.fill = GridBagConstraints.HORIZONTAL;
		gbc_textField_14.insets = new Insets(0, 0, 5, 0);
		gbc_textField_14.gridx = 3;
		gbc_textField_14.gridy = 7;
		defectsDetailsPanel.add(textField_14, gbc_textField_14);
		
		lblRelease = new JXLabel();
		lblRelease.setText("Release");
		lblRelease.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblRelease.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblRelease = new GridBagConstraints();
		gbc_lblRelease.anchor = GridBagConstraints.WEST;
		gbc_lblRelease.insets = new Insets(0, 0, 5, 5);
		gbc_lblRelease.gridx = 0;
		gbc_lblRelease.gridy = 8;
		defectsDetailsPanel.add(lblRelease, gbc_lblRelease);
		
		txtfldRelease = new JXTextField();
		txtfldRelease.setOpaque(false);
		txtfldRelease.setEditable(false);
		txtfldRelease.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldRelease.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldRelease = new GridBagConstraints();
		gbc_txtfldRelease.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldRelease.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldRelease.gridx = 1;
		gbc_txtfldRelease.gridy = 8;
		defectsDetailsPanel.add(txtfldRelease, gbc_txtfldRelease);

		lblDetectedInBuild = new JXLabel();
		lblDetectedInBuild.setText("Detected in Build");
		lblDetectedInBuild.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblDetectedInBuild.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lblDetectedInBuild = new GridBagConstraints();
		gbc_lblDetectedInBuild.anchor = GridBagConstraints.WEST;
		gbc_lblDetectedInBuild.insets = new Insets(0, 0, 5, 5);
		gbc_lblDetectedInBuild.gridx = 2;
		gbc_lblDetectedInBuild.gridy = 8;
		defectsDetailsPanel.add(lblDetectedInBuild, gbc_lblDetectedInBuild);

		txtfldDetectedInBuild = new JXTextField();
		txtfldDetectedInBuild.setOpaque(false);
		txtfldDetectedInBuild.setEditable(false);
		txtfldDetectedInBuild.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldDetectedInBuild.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldDetectedInBuild = new GridBagConstraints();
		gbc_txtfldDetectedInBuild.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldDetectedInBuild.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldDetectedInBuild.gridx = 3;
		gbc_txtfldDetectedInBuild.gridy = 8;
		defectsDetailsPanel.add(txtfldDetectedInBuild, gbc_txtfldDetectedInBuild);
		
		lblSprint = new JXLabel();
		lblSprint.setText("Sprint");
		lblSprint.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblSprint.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lblSprint = new GridBagConstraints();
		gbc_lblSprint.anchor = GridBagConstraints.WEST;
		gbc_lblSprint.insets = new Insets(0, 0, 5, 5);
		gbc_lblSprint.gridx = 0;
		gbc_lblSprint.gridy = 9;
		defectsDetailsPanel.add(lblSprint, gbc_lblSprint);
		
		txtfldSprint = new JXTextField();
		txtfldSprint.setOpaque(false);
		txtfldSprint.setEditable(false);
		txtfldSprint.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldSprint.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldSprint = new GridBagConstraints();
		gbc_txtfldSprint.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldSprint.insets = new Insets(0, 0, 5, 5);
		gbc_txtfldSprint.gridx = 1;
		gbc_txtfldSprint.gridy = 9;
		defectsDetailsPanel.add(txtfldSprint, gbc_txtfldSprint);
		
		lblDetectedInRelease = new JXLabel();
		lblDetectedInRelease.setText("Detected in release");
		lblDetectedInRelease.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblDetectedInRelease.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lblDetectedInRelease = new GridBagConstraints();
		gbc_lblDetectedInRelease.anchor = GridBagConstraints.WEST;
		gbc_lblDetectedInRelease.insets = new Insets(0, 0, 5, 5);
		gbc_lblDetectedInRelease.gridx = 2;
		gbc_lblDetectedInRelease.gridy = 9;
		defectsDetailsPanel.add(lblDetectedInRelease, gbc_lblDetectedInRelease);
		
		txtfldDetectedInRelease = new JXTextField();
		txtfldDetectedInRelease.setOpaque(false);
		txtfldDetectedInRelease.setEditable(false);
		txtfldDetectedInRelease.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldDetectedInRelease.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldDetectedInRelease = new GridBagConstraints();
		gbc_txtfldDetectedInRelease.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldDetectedInRelease.insets = new Insets(0, 0, 5, 0);
		gbc_txtfldDetectedInRelease.gridx = 3;
		gbc_txtfldDetectedInRelease.gridy = 9;
		defectsDetailsPanel.add(txtfldDetectedInRelease, gbc_txtfldDetectedInRelease);

		lblFixedInBuild = new JXLabel();
		lblFixedInBuild.setText("Fixed in Build");
		lblFixedInBuild.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblFixedInBuild.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lblFixedInBuild = new GridBagConstraints();
		gbc_lblFixedInBuild.anchor = GridBagConstraints.WEST;
		gbc_lblFixedInBuild.insets = new Insets(0, 0, 0, 5);
		gbc_lblFixedInBuild.gridx = 2;
		gbc_lblFixedInBuild.gridy = 10;
		defectsDetailsPanel.add(lblFixedInBuild, gbc_lblFixedInBuild);

		txtfldFixedInBuild = new JXTextField();
		txtfldFixedInBuild.setOpaque(false);
		txtfldFixedInBuild.setEditable(false);
		txtfldFixedInBuild.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		txtfldFixedInBuild.setBackground(new Color(0, 0, 0, 0));
		GridBagConstraints gbc_txtfldFixedInBuild = new GridBagConstraints();
		gbc_txtfldFixedInBuild.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtfldFixedInBuild.gridx = 3;
		gbc_txtfldFixedInBuild.gridy = 10;
		defectsDetailsPanel.add(txtfldFixedInBuild, gbc_txtfldFixedInBuild);

		atachementsPanel = new JXPanel();
		atachementsPanel.setBorder(new MatteBorder(1, 0, 0, 0, JBColor.border()));
		GridBagConstraints gbc_atachementsPanel = new GridBagConstraints();
		gbc_atachementsPanel.fill = GridBagConstraints.BOTH;
		gbc_atachementsPanel.gridx = 0;
		gbc_atachementsPanel.gridy = 4;
		rootPanel.add(atachementsPanel, gbc_atachementsPanel);
		GridBagLayout gbl_atachementsPanel = new GridBagLayout();
		gbl_atachementsPanel.columnWidths = new int[]{0, 0, 0};
		gbl_atachementsPanel.rowHeights = new int[]{0, 0};
		gbl_atachementsPanel.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
		gbl_atachementsPanel.rowWeights = new double[]{0.0, Double.MIN_VALUE};
		atachementsPanel.setLayout(gbl_atachementsPanel);
		
		lblAttachments = new JXLabel();
		lblAttachments.setText("Attachments");
		lblAttachments.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblAttachments.setBorder(new EmptyBorder(5, 0, 0, 10));
		GridBagConstraints gbc_lblAttachments = new GridBagConstraints();
		gbc_lblAttachments.anchor = GridBagConstraints.WEST;
		gbc_lblAttachments.insets = new Insets(0, 0, 0, 5);
		gbc_lblAttachments.gridx = 0;
		gbc_lblAttachments.gridy = 0;
		atachementsPanel.add(lblAttachments, gbc_lblAttachments);

		listOfAttachments = new JXLabel();
		listOfAttachments.setText("No attachments");
		listOfAttachments.setLineWrap(true);
		listOfAttachments.setBorder(new EmptyBorder(5, 0, 0, 0));
		GridBagConstraints gbc_listOfAttachments = new GridBagConstraints();
		gbc_listOfAttachments.fill = GridBagConstraints.HORIZONTAL;
		gbc_listOfAttachments.gridx = 1;
		gbc_listOfAttachments.gridy = 0;
		atachementsPanel.add(listOfAttachments, gbc_listOfAttachments);
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
		this.txtfldPriority.setText(priority);
	}

	public void setTxtfldRelease(String release) {
		this.txtfldRelease.setText(release);
	}

	public void setTextfldFixedInPush(String fixedInPush) {
		this.txtfldFixedInBuild.setText(fixedInPush);
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
		this.txtfldStoryPoints.setText(storyPoints);
	}

	public void setTextfldDetectedInPush(String detectedInPush) {
		this.txtfldDetectedInBuild.setText(detectedInPush);
	}

	public void setTxtfldDetectedInRelease(String detectedInRelease) {
		this.txtfldDetectedInRelease.setText(detectedInRelease);
	}

	public String getComboBoxPhase() {
		return comboBoxPhase.getSelectedItem().toString();
	}

	void setComboBoxPhase(String phaseName) {
		this.comboBoxPhase.getEditor().setItem(phaseName);
//		phases.entrySet().stream().filter(entry -> phaseName.equals(entry.getKey())).forEach(entry -> {
//			this.comboBoxPhase.setSelectedIndex(1);
//		});
	}

	public String getTxtfldBlocked() {
		return txtfldBlocked.getText();
	}

	public void setTxtfldBlocked(String txtfldBlocked) {
        txtfldBlocked = (txtfldBlocked == "true") ? "Yes" : "No";
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

	public void setLblName(String lblName) {
		this.lblName.setText(lblName);
	}

}
