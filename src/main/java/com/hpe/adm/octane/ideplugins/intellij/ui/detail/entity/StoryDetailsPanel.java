/*
 * Copyright 2017 Hewlett-Packard Enterprise Development Company, L.P.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entity;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.Objects;

public class StoryDetailsPanel extends JXPanel {
	private static final long serialVersionUID = -7172388625845199450L;
	private JXLabel ownerDetails;
	private JXLabel featureDetails;
	private JXLabel sprintDetails;
	private JXLabel storyPointsDetails;
	private JXLabel blockedDetails;
	private JXLabel authorDetails;
	private JXLabel releaseDetails;
	private JXLabel lastModifiedDetails;
	private JXLabel teamDetails;
	private JXLabel blockedReasonDetails;
	private JXLabel appModuleDetails;
	private JXLabel creationTimeDetails;
	private JXLabel itemOriginDetails;
	protected final JXPanel detailsPanelLeft;
	private JXLabel varfieldLabel;
	private boolean isUserStory;
	private JXLabel varFieldValueDetails;

	public StoryDetailsPanel(boolean isUserStory) {
		this.isUserStory = isUserStory;
		setBorder(null);
		setLayout(new BorderLayout(0, 0));

		JXPanel detailsPanelMain = new JXPanel();
		detailsPanelMain.setBorder(null);
		add(detailsPanelMain, BorderLayout.CENTER);
		detailsPanelMain.setLayout(new BorderLayout(0, 0));
		detailsPanelMain.setMinimumSize(new Dimension(0, 0));

		detailsPanelLeft = new JXPanel();
		detailsPanelLeft.setBorder(null);
		detailsPanelMain.add(detailsPanelLeft, BorderLayout.WEST);
		GridBagLayout gbl_detailsPanelLeft = new GridBagLayout();
		gbl_detailsPanelLeft.columnWidths = new int[] { 0, 0 };
		gbl_detailsPanelLeft.rowHeights = new int[] { 0, 0, 0, 0, 0, 0, 0, 0 };
		gbl_detailsPanelLeft.columnWeights = new double[] { 0.0, 1.0 };
		gbl_detailsPanelLeft.rowWeights = new double[] { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE };
		detailsPanelLeft.setLayout(gbl_detailsPanelLeft);

		JXLabel ownerLabel = new JXLabel();
		GridBagConstraints gbc_ownerLabel = new GridBagConstraints();
		gbc_ownerLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_ownerLabel.insets = new Insets(0, 0, 5, 5);
		gbc_ownerLabel.gridx = 0;
		gbc_ownerLabel.gridy = 0;
		detailsPanelLeft.add(ownerLabel, gbc_ownerLabel);
		ownerLabel.setFont(new Font("Arial", Font.BOLD, 12));
		ownerLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		ownerLabel.setText("Owner");

		ownerDetails = new JXLabel();
		ownerDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		GridBagConstraints gbc_ownerDetails = new GridBagConstraints();
		gbc_ownerDetails.anchor = GridBagConstraints.SOUTH;
		gbc_ownerDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_ownerDetails.insets = new Insets(0, 0, 5, 0);
		gbc_ownerDetails.gridx = 1;
		gbc_ownerDetails.gridy = 0;
		detailsPanelLeft.add(ownerDetails, gbc_ownerDetails);
		ownerDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		ownerDetails.setText("                ");

		JXLabel featureLabel = new JXLabel();
		featureLabel.setText("Feature");
		featureLabel.setFont(new Font("Arial", Font.BOLD, 12));
		featureLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_featureLabel = new GridBagConstraints();
		gbc_featureLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_featureLabel.insets = new Insets(0, 0, 5, 5);
		gbc_featureLabel.gridx = 0;
		gbc_featureLabel.gridy = 1;
		detailsPanelLeft.add(featureLabel, gbc_featureLabel);

		featureDetails = new JXLabel();
		featureDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		featureDetails.setText("                ");
		featureDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_featureDetails = new GridBagConstraints();
		gbc_featureDetails.anchor = GridBagConstraints.SOUTH;
		gbc_featureDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_featureDetails.insets = new Insets(0, 0, 5, 0);
		gbc_featureDetails.gridx = 1;
		gbc_featureDetails.gridy = 1;
		detailsPanelLeft.add(featureDetails, gbc_featureDetails);

		JXLabel sprintLabel = new JXLabel();
		sprintLabel.setText("Sprint");
		sprintLabel.setFont(new Font("Arial", Font.BOLD, 12));
		sprintLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_sprintLabel = new GridBagConstraints();
		gbc_sprintLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_sprintLabel.insets = new Insets(0, 0, 5, 5);
		gbc_sprintLabel.gridx = 0;
		gbc_sprintLabel.gridy = 2;
		detailsPanelLeft.add(sprintLabel, gbc_sprintLabel);

		sprintDetails = new JXLabel();
		sprintDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		sprintDetails.setText("                ");
		sprintDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_sprintDetails = new GridBagConstraints();
		gbc_sprintDetails.anchor = GridBagConstraints.SOUTH;
		gbc_sprintDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_sprintDetails.insets = new Insets(0, 0, 5, 0);
		gbc_sprintDetails.gridx = 1;
		gbc_sprintDetails.gridy = 2;
		detailsPanelLeft.add(sprintDetails, gbc_sprintDetails);

		JXLabel storyPointsLabel = new JXLabel();
		storyPointsLabel.setText("Story points");
		storyPointsLabel.setFont(new Font("Arial", Font.BOLD, 12));
		storyPointsLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_storyPointsLabel = new GridBagConstraints();
		gbc_storyPointsLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_storyPointsLabel.insets = new Insets(0, 0, 5, 5);
		gbc_storyPointsLabel.gridx = 0;
		gbc_storyPointsLabel.gridy = 3;
		detailsPanelLeft.add(storyPointsLabel, gbc_storyPointsLabel);

		storyPointsDetails = new JXLabel();
		storyPointsDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		storyPointsDetails.setText("                ");
		storyPointsDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_storyPointsDetails = new GridBagConstraints();
		gbc_storyPointsDetails.anchor = GridBagConstraints.SOUTH;
		gbc_storyPointsDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_storyPointsDetails.insets = new Insets(0, 0, 5, 0);
		gbc_storyPointsDetails.gridx = 1;
		gbc_storyPointsDetails.gridy = 3;
		detailsPanelLeft.add(storyPointsDetails, gbc_storyPointsDetails);

		JXLabel blockedLabel = new JXLabel();
		GridBagConstraints gbc_blockedLabel = new GridBagConstraints();
		gbc_blockedLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_blockedLabel.insets = new Insets(0, 0, 5, 5);
		gbc_blockedLabel.gridx = 0;
		gbc_blockedLabel.gridy = 4;
		detailsPanelLeft.add(blockedLabel, gbc_blockedLabel);
		blockedLabel.setText("Blocked");
		blockedLabel.setFont(new Font("Arial", Font.BOLD, 12));
		blockedLabel.setBorder(new EmptyBorder(0, 0, 0, 10));

		blockedDetails = new JXLabel();
		blockedDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		blockedDetails.setText("                ");
		blockedDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_blockedDetails = new GridBagConstraints();
		gbc_blockedDetails.anchor = GridBagConstraints.SOUTH;
		gbc_blockedDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_blockedDetails.insets = new Insets(0, 0, 5, 0);
		gbc_blockedDetails.gridx = 1;
		gbc_blockedDetails.gridy = 4;
		detailsPanelLeft.add(blockedDetails, gbc_blockedDetails);

		varfieldLabel = new JXLabel();
		varfieldLabel.setText("varField");
		varfieldLabel.setFont(new Font("Arial", Font.BOLD, 12));
		varfieldLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_varfieldLabel = new GridBagConstraints();
		gbc_varfieldLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_varfieldLabel.insets = new Insets(0, 0, 5, 5);
		gbc_varfieldLabel.gridx = 0;
		gbc_varfieldLabel.gridy = 5;
		detailsPanelLeft.add(varfieldLabel, gbc_varfieldLabel);

		varFieldValueDetails = new JXLabel();
		varFieldValueDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		varFieldValueDetails.setText("                ");
		varFieldValueDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_varFieldValueDetails = new GridBagConstraints();
		gbc_varFieldValueDetails.anchor = GridBagConstraints.SOUTH;
		gbc_varFieldValueDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_varFieldValueDetails.insets = new Insets(0, 0, 5, 0);
		gbc_varFieldValueDetails.gridx = 1;
		gbc_varFieldValueDetails.gridy = 5;
		detailsPanelLeft.add(varFieldValueDetails, gbc_varFieldValueDetails);

		JXLabel lastModifiedLabel = new JXLabel();
		lastModifiedLabel.setText("Last modified");
		lastModifiedLabel.setFont(new Font("Arial", Font.BOLD, 12));
		lastModifiedLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_lastModifiedLabel = new GridBagConstraints();
		gbc_lastModifiedLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_lastModifiedLabel.insets = new Insets(0, 0, 0, 5);
		gbc_lastModifiedLabel.gridx = 0;
		gbc_lastModifiedLabel.gridy = 6;
		detailsPanelLeft.add(lastModifiedLabel, gbc_lastModifiedLabel);

		lastModifiedDetails = new JXLabel();
		lastModifiedDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		lastModifiedDetails.setText("                ");
		lastModifiedDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_lastModifiedDetails = new GridBagConstraints();
		gbc_lastModifiedDetails.anchor = GridBagConstraints.SOUTH;
		gbc_lastModifiedDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_lastModifiedDetails.gridx = 1;
		gbc_lastModifiedDetails.gridy = 6;
		detailsPanelLeft.add(lastModifiedDetails, gbc_lastModifiedDetails);

		JXPanel detailsPanelRight = new JXPanel();
		detailsPanelRight.setBorder(null);
		detailsPanelMain.add(detailsPanelRight, BorderLayout.CENTER);
		GridBagLayout gbl_detailsPanelRight = new GridBagLayout();
		gbl_detailsPanelRight.columnWidths = new int[] { 0, 0, 0 };
		gbl_detailsPanelRight.rowHeights = new int[] { 0, 0, 0, 0, 0, 0, 0, 0 };
		gbl_detailsPanelRight.columnWeights = new double[] { 0.0, 1.0, Double.MIN_VALUE };
		gbl_detailsPanelRight.rowWeights = new double[] { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE };
		detailsPanelRight.setLayout(gbl_detailsPanelRight);

		JXLabel teamLabel = new JXLabel();
		teamLabel.setText("Team");
		teamLabel.setFont(new Font("Arial", Font.BOLD, 12));
		teamLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_teamLabel = new GridBagConstraints();
		gbc_teamLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_teamLabel.insets = new Insets(0, 0, 5, 5);
		gbc_teamLabel.gridx = 0;
		gbc_teamLabel.gridy = 0;
		detailsPanelRight.add(teamLabel, gbc_teamLabel);

		teamDetails = new JXLabel();
		teamDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		teamDetails.setText("                ");
		teamDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_teamDetails = new GridBagConstraints();
		gbc_teamDetails.anchor = GridBagConstraints.SOUTH;
		gbc_teamDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_teamDetails.insets = new Insets(0, 0, 5, 0);
		gbc_teamDetails.gridx = 1;
		gbc_teamDetails.gridy = 0;
		detailsPanelRight.add(teamDetails, gbc_teamDetails);

		JXLabel authorLabel = new JXLabel();
		GridBagConstraints gbc_authorLabel = new GridBagConstraints();
		gbc_authorLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_authorLabel.insets = new Insets(0, 0, 5, 5);
		gbc_authorLabel.gridx = 0;
		gbc_authorLabel.gridy = 1;
		detailsPanelRight.add(authorLabel, gbc_authorLabel);
		authorLabel.setText("Author");
		authorLabel.setFont(new Font("Arial", Font.BOLD, 12));
		authorLabel.setBorder(new EmptyBorder(0, 30, 0, 10));

		authorDetails = new JXLabel();
		authorDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		GridBagConstraints gbc_authorDetails = new GridBagConstraints();
		gbc_authorDetails.anchor = GridBagConstraints.SOUTH;
		gbc_authorDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_authorDetails.insets = new Insets(0, 0, 5, 0);
		gbc_authorDetails.gridx = 1;
		gbc_authorDetails.gridy = 1;
		detailsPanelRight.add(authorDetails, gbc_authorDetails);
		authorDetails.setText("                ");
		authorDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));

		JXLabel appModulesLabel = new JXLabel();
		appModulesLabel.setText("Application module");
		appModulesLabel.setFont(new Font("Arial", Font.BOLD, 12));
		appModulesLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_appModulesLabel = new GridBagConstraints();
		gbc_appModulesLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_appModulesLabel.insets = new Insets(0, 0, 5, 5);
		gbc_appModulesLabel.gridx = 0;
		gbc_appModulesLabel.gridy = 2;
		detailsPanelRight.add(appModulesLabel, gbc_appModulesLabel);

		appModuleDetails = new JXLabel();
		appModuleDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		appModuleDetails.setText("                ");
		appModuleDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_appModuleDetails = new GridBagConstraints();
		gbc_appModuleDetails.anchor = GridBagConstraints.SOUTH;
		gbc_appModuleDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_appModuleDetails.insets = new Insets(0, 0, 5, 0);
		gbc_appModuleDetails.gridx = 1;
		gbc_appModuleDetails.gridy = 2;
		detailsPanelRight.add(appModuleDetails, gbc_appModuleDetails);

		JXLabel itemOriginLabel = new JXLabel();
		itemOriginLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
		itemOriginLabel.setFont(new Font("Arial", Font.BOLD, 12));
		itemOriginLabel.setText("Item origin");
		GridBagConstraints gbc_itemOriginLabel = new GridBagConstraints();
		gbc_itemOriginLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_itemOriginLabel.insets = new Insets(0, 0, 5, 5);
		gbc_itemOriginLabel.gridx = 0;
		gbc_itemOriginLabel.gridy = 3;
		detailsPanelRight.add(itemOriginLabel, gbc_itemOriginLabel);

		itemOriginDetails = new JXLabel();
		itemOriginDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		itemOriginDetails.setText("                ");
		itemOriginDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_itemOriginDetails = new GridBagConstraints();
		gbc_itemOriginDetails.anchor = GridBagConstraints.SOUTH;
		gbc_itemOriginDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_itemOriginDetails.insets = new Insets(0, 0, 5, 0);
		gbc_itemOriginDetails.gridx = 1;
		gbc_itemOriginDetails.gridy = 3;
		detailsPanelRight.add(itemOriginDetails, gbc_itemOriginDetails);

		JXLabel blockedReasonLabel = new JXLabel();
		blockedReasonLabel.setText("Blocked reason");
		blockedReasonLabel.setFont(new Font("Arial", Font.BOLD, 12));
		blockedReasonLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_blockedReasonLabel = new GridBagConstraints();
		gbc_blockedReasonLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_blockedReasonLabel.insets = new Insets(0, 0, 5, 5);
		gbc_blockedReasonLabel.gridx = 0;
		gbc_blockedReasonLabel.gridy = 4;
		detailsPanelRight.add(blockedReasonLabel, gbc_blockedReasonLabel);

		blockedReasonDetails = new JXLabel();
		blockedReasonDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		blockedReasonDetails.setText("                ");
		blockedReasonDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_blockedReasonDetails = new GridBagConstraints();
		gbc_blockedReasonDetails.anchor = GridBagConstraints.SOUTH;
		gbc_blockedReasonDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_blockedReasonDetails.insets = new Insets(0, 0, 5, 0);
		gbc_blockedReasonDetails.gridx = 1;
		gbc_blockedReasonDetails.gridy = 4;
		detailsPanelRight.add(blockedReasonDetails, gbc_blockedReasonDetails);

		JXLabel releaseLabel = new JXLabel();
		GridBagConstraints gbc_releaseLabel = new GridBagConstraints();
		gbc_releaseLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_releaseLabel.insets = new Insets(0, 0, 5, 5);
		gbc_releaseLabel.gridx = 0;
		gbc_releaseLabel.gridy = 5;
		detailsPanelRight.add(releaseLabel, gbc_releaseLabel);
		releaseLabel.setText("Release");
		releaseLabel.setFont(new Font("Arial", Font.BOLD, 12));
		releaseLabel.setBorder(new EmptyBorder(0, 30, 0, 10));

		releaseDetails = new JXLabel();
		releaseDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		GridBagConstraints gbc_releaseDetails = new GridBagConstraints();
		gbc_releaseDetails.anchor = GridBagConstraints.SOUTH;
		gbc_releaseDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_releaseDetails.insets = new Insets(0, 0, 5, 0);
		gbc_releaseDetails.gridx = 1;
		gbc_releaseDetails.gridy = 5;
		detailsPanelRight.add(releaseDetails, gbc_releaseDetails);
		releaseDetails.setText("                ");
		releaseDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));

		JXLabel creationTimeLabel = new JXLabel();
		creationTimeLabel.setText("Creation time");
		creationTimeLabel.setFont(new Font("Arial", Font.BOLD, 12));
		creationTimeLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_creationTimeLabel = new GridBagConstraints();
		gbc_creationTimeLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_creationTimeLabel.insets = new Insets(0, 0, 0, 5);
		gbc_creationTimeLabel.gridx = 0;
		gbc_creationTimeLabel.gridy = 6;
		detailsPanelRight.add(creationTimeLabel, gbc_creationTimeLabel);

		creationTimeDetails = new JXLabel();
		creationTimeDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		creationTimeDetails.setText("                ");
		creationTimeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_creationTimeDetails = new GridBagConstraints();
		gbc_creationTimeDetails.anchor = GridBagConstraints.SOUTH;
		gbc_creationTimeDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_creationTimeDetails.gridx = 1;
		gbc_creationTimeDetails.gridy = 6;
		detailsPanelRight.add(creationTimeDetails, gbc_creationTimeDetails);

		addComponentListener(new ComponentAdapter() {
			public void componentResized(ComponentEvent e) {
				int halfWidth = detailsPanelMain.getWidth() / 2;
				int height = detailsPanelMain.getHeight();

				if (halfWidth != 0 && height != 0) {
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

	public void setFeatureDetails(String featureDetails) {
		this.featureDetails.setText(featureDetails);
	}

	public void setSprintDetails(String sprintDetails) {
		this.sprintDetails.setText(sprintDetails);
	}

	public void setStoryPointsDetails(String storyPointsDetails) {
		this.storyPointsDetails.setText(storyPointsDetails);
	}

	public void setBlockedDetails(String blockedDetails) {
		blockedDetails = (Objects.equals("true", blockedDetails)) ? "yes" : "no";
		this.blockedDetails.setText(blockedDetails);
	}

	public void setAuthorDetailsDetails(String authorDetails) {
		this.authorDetails.setText(authorDetails);
	}

	public void setReleaseDetails(String releaseDetails) {
		this.releaseDetails.setText(releaseDetails);
	}

	public void setLastModifiedDetails(String lastModifiedDetails) {
		this.lastModifiedDetails.setText(lastModifiedDetails);
	}

	public void setTeamDetails(String teamDetails) {
		this.teamDetails.setText(teamDetails);
	}

	public void setBlockedReasonDetails(String blockedReasonDetails) {
		this.blockedReasonDetails.setText(blockedReasonDetails);
	}

	public void setAppModuleDetails(String appModuleDetails) {
		this.appModuleDetails.setText(appModuleDetails);
	}

	public void setCreationTimeDetails(String creationTimeDetails) {
		this.creationTimeDetails.setText(creationTimeDetails);
	}

	public void setItemOriginDetails(String itemOriginDetails) {
		this.itemOriginDetails.setText(itemOriginDetails);
	}

	public void setVarItemValue(String varItemValues) {
		if (isUserStory) {
			this.varfieldLabel.setText("Last runs");
		} else {
			this.varfieldLabel.setText("Quality story type");
		}
		this.varFieldValueDetails.setText(varItemValues);
	}
}
