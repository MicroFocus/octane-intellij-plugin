/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
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

public class SuiteTestRunDetailsPanel extends JXPanel {
	private static final long serialVersionUID = -7172388625845199450L;
	private JXLabel startedTimeDetails;
	private JXLabel suiteTestNameDetails;
	private JXLabel defaultRunByDetails;
	private JXLabel authorDetails;
	private JXLabel contentDetails;
	private JXLabel releaseDetails;
	private JXLabel nativeStatusDetails;
	private JXLabel draftRunDetails;
	private JXLabel lastModifiedDetails;
	private JXLabel environmentDetails;

	public SuiteTestRunDetailsPanel() {
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
		gbl_detailsPanelLeft.columnWidths = new int[] { 0, 0 };
		gbl_detailsPanelLeft.rowHeights = new int[] { 0, 0, 0, 0, 0, 0 };
		gbl_detailsPanelLeft.columnWeights = new double[] { 0.0, 1.0 };
		gbl_detailsPanelLeft.rowWeights = new double[] { 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE };
		detailsPanelLeft.setLayout(gbl_detailsPanelLeft);

		JXLabel suiteNameLabel = new JXLabel();
		suiteNameLabel.setText("Suite Name");
		suiteNameLabel.setFont(new Font("Arial", Font.BOLD, 12));
		suiteNameLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_suiteNameLabel = new GridBagConstraints();
		gbc_suiteNameLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_suiteNameLabel.insets = new Insets(0, 0, 5, 5);
		gbc_suiteNameLabel.gridx = 0;
		gbc_suiteNameLabel.gridy = 0;
		detailsPanelLeft.add(suiteNameLabel, gbc_suiteNameLabel);

		suiteTestNameDetails = new JXLabel();
		suiteTestNameDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		suiteTestNameDetails.setText("                                 ");
		suiteTestNameDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_suiteTestNameDetails = new GridBagConstraints();
		gbc_suiteTestNameDetails.anchor = GridBagConstraints.SOUTH;
		gbc_suiteTestNameDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_suiteTestNameDetails.insets = new Insets(0, 0, 5, 0);
		gbc_suiteTestNameDetails.gridx = 1;
		gbc_suiteTestNameDetails.gridy = 0;
		detailsPanelLeft.add(suiteTestNameDetails, gbc_suiteTestNameDetails);

		JXLabel defaultRunByLabel = new JXLabel();
		defaultRunByLabel.setText("Default run by");
		defaultRunByLabel.setFont(new Font("Arial", Font.BOLD, 12));
		defaultRunByLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		GridBagConstraints gbc_defaultRunByLabel = new GridBagConstraints();
		gbc_defaultRunByLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_defaultRunByLabel.insets = new Insets(0, 0, 5, 5);
		gbc_defaultRunByLabel.gridx = 0;
		gbc_defaultRunByLabel.gridy = 1;
		detailsPanelLeft.add(defaultRunByLabel, gbc_defaultRunByLabel);

		defaultRunByDetails = new JXLabel();
		defaultRunByDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		defaultRunByDetails.setText("                                 ");
		defaultRunByDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_defaultRunByDetails = new GridBagConstraints();
		gbc_defaultRunByDetails.anchor = GridBagConstraints.SOUTH;
		gbc_defaultRunByDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_defaultRunByDetails.insets = new Insets(0, 0, 5, 0);
		gbc_defaultRunByDetails.gridx = 1;
		gbc_defaultRunByDetails.gridy = 1;
		detailsPanelLeft.add(defaultRunByDetails, gbc_defaultRunByDetails);

		JXLabel startedDateLabel = new JXLabel();
		GridBagConstraints gbc_startedDateLabel = new GridBagConstraints();
		gbc_startedDateLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_startedDateLabel.insets = new Insets(0, 0, 5, 5);
		gbc_startedDateLabel.gridx = 0;
		gbc_startedDateLabel.gridy = 2;
		detailsPanelLeft.add(startedDateLabel, gbc_startedDateLabel);
		startedDateLabel.setFont(new Font("Arial", Font.BOLD, 12));
		startedDateLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		startedDateLabel.setText("Started");

		startedTimeDetails = new JXLabel();
		startedTimeDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		GridBagConstraints gbc_startedTimeDetails = new GridBagConstraints();
		gbc_startedTimeDetails.anchor = GridBagConstraints.SOUTH;
		gbc_startedTimeDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_startedTimeDetails.insets = new Insets(0, 0, 5, 0);
		gbc_startedTimeDetails.gridx = 1;
		gbc_startedTimeDetails.gridy = 2;
		detailsPanelLeft.add(startedTimeDetails, gbc_startedTimeDetails);
		startedTimeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		startedTimeDetails.setText("                                 ");

		JXLabel contentLabel = new JXLabel();
		contentLabel.setFont(new Font("Arial", Font.BOLD, 12));
		contentLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		contentLabel.setText("Content");
		GridBagConstraints gbc_contentLabel = new GridBagConstraints();
		gbc_contentLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_contentLabel.insets = new Insets(0, 0, 5, 5);
		gbc_contentLabel.gridx = 0;
		gbc_contentLabel.gridy = 3;
		detailsPanelLeft.add(contentLabel, gbc_contentLabel);

		contentDetails = new JXLabel();
		contentDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		contentDetails.setText("                                 ");
		contentDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_contentDetails = new GridBagConstraints();
		gbc_contentDetails.anchor = GridBagConstraints.SOUTH;
		gbc_contentDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_contentDetails.insets = new Insets(0, 0, 5, 0);
		gbc_contentDetails.gridx = 1;
		gbc_contentDetails.gridy = 3;
		detailsPanelLeft.add(contentDetails, gbc_contentDetails);

		JXLabel releaseLabel = new JXLabel();
		releaseLabel.setFont(new Font("Arial", Font.BOLD, 12));
		releaseLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
		releaseLabel.setText("Release");
		GridBagConstraints gbc_releaseLabel = new GridBagConstraints();
		gbc_releaseLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_releaseLabel.insets = new Insets(0, 0, 0, 5);
		gbc_releaseLabel.gridx = 0;
		gbc_releaseLabel.gridy = 4;
		detailsPanelLeft.add(releaseLabel, gbc_releaseLabel);

		releaseDetails = new JXLabel();
		releaseDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		releaseDetails.setText("                                 ");
		releaseDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_releaseDetails = new GridBagConstraints();
		gbc_releaseDetails.anchor = GridBagConstraints.SOUTH;
		gbc_releaseDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_releaseDetails.gridx = 1;
		gbc_releaseDetails.gridy = 4;
		detailsPanelLeft.add(releaseDetails, gbc_releaseDetails);

		JXPanel detailsPanelRight = new JXPanel();
		detailsPanelRight.setBorder(null);
		detailsPanelMain.add(detailsPanelRight, BorderLayout.CENTER);
		GridBagLayout gbl_detailsPanelRight = new GridBagLayout();
		gbl_detailsPanelRight.columnWidths = new int[] { 0, 0, 0 };
		gbl_detailsPanelRight.rowHeights = new int[] { 0, 0, 0, 0, 0, 0 };
		gbl_detailsPanelRight.columnWeights = new double[] { 0.0, 1.0, Double.MIN_VALUE };
		gbl_detailsPanelRight.rowWeights = new double[] { 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE };
		detailsPanelRight.setLayout(gbl_detailsPanelRight);

		JXLabel nativeStatusLabel = new JXLabel();
		nativeStatusLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
		nativeStatusLabel.setFont(new Font("Arial", Font.BOLD, 12));
		nativeStatusLabel.setText("Native status");
		GridBagConstraints gbc_nativeStatusLabel = new GridBagConstraints();
		gbc_nativeStatusLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_nativeStatusLabel.insets = new Insets(0, 0, 5, 5);
		gbc_nativeStatusLabel.gridx = 0;
		gbc_nativeStatusLabel.gridy = 0;
		detailsPanelRight.add(nativeStatusLabel, gbc_nativeStatusLabel);

		nativeStatusDetails = new JXLabel();
		nativeStatusDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		nativeStatusDetails.setText("                  ");
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
		authorDetails.setText("                  ");
		authorDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));

		JXLabel lastModifiedLabel = new JXLabel();
		lastModifiedLabel.setText("Last modified");
		lastModifiedLabel.setFont(new Font("Arial", Font.BOLD, 12));
		lastModifiedLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
		GridBagConstraints gbc_lastModifiedLabel = new GridBagConstraints();
		gbc_lastModifiedLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_lastModifiedLabel.insets = new Insets(0, 0, 5, 5);
		gbc_lastModifiedLabel.gridx = 0;
		gbc_lastModifiedLabel.gridy = 2;
		detailsPanelRight.add(lastModifiedLabel, gbc_lastModifiedLabel);

		lastModifiedDetails = new JXLabel();
		lastModifiedDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		lastModifiedDetails.setText("                  ");
		lastModifiedDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_lastModifiedDetails = new GridBagConstraints();
		gbc_lastModifiedDetails.anchor = GridBagConstraints.SOUTH;
		gbc_lastModifiedDetails.insets = new Insets(0, 0, 5, 0);
		gbc_lastModifiedDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_lastModifiedDetails.gridx = 1;
		gbc_lastModifiedDetails.gridy = 2;
		detailsPanelRight.add(lastModifiedDetails, gbc_lastModifiedDetails);

		JXLabel draftRunLabel = new JXLabel();
		draftRunLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
		draftRunLabel.setFont(new Font("Arial", Font.BOLD, 12));
		draftRunLabel.setText("Draft run");
		GridBagConstraints gbc_draftRunLabel = new GridBagConstraints();
		gbc_draftRunLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_draftRunLabel.insets = new Insets(0, 0, 5, 5);
		gbc_draftRunLabel.gridx = 0;
		gbc_draftRunLabel.gridy = 3;
		detailsPanelRight.add(draftRunLabel, gbc_draftRunLabel);

		draftRunDetails = new JXLabel();
		draftRunDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		draftRunDetails.setText("                  ");
		draftRunDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
		GridBagConstraints gbc_draftRunDetails = new GridBagConstraints();
		gbc_draftRunDetails.insets = new Insets(0, 0, 5, 0);
		gbc_draftRunDetails.anchor = GridBagConstraints.SOUTH;
		gbc_draftRunDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_draftRunDetails.gridx = 1;
		gbc_draftRunDetails.gridy = 3;
		detailsPanelRight.add(draftRunDetails, gbc_draftRunDetails);

		JXLabel environmentLabel = new JXLabel();
		GridBagConstraints gbc_environmentLabel = new GridBagConstraints();
		gbc_environmentLabel.anchor = GridBagConstraints.SOUTHWEST;
		gbc_environmentLabel.insets = new Insets(0, 0, 0, 5);
		gbc_environmentLabel.gridx = 0;
		gbc_environmentLabel.gridy = 4;
		detailsPanelRight.add(environmentLabel, gbc_environmentLabel);
		environmentLabel.setText("Environment");
		environmentLabel.setFont(new Font("Arial", Font.BOLD, 12));
		environmentLabel.setBorder(new EmptyBorder(0, 30, 0, 10));

		environmentDetails = new JXLabel();
		environmentDetails.setFont(new Font("Arial", Font.PLAIN, 12));
		GridBagConstraints gbc_environmentDetails = new GridBagConstraints();
		gbc_environmentDetails.anchor = GridBagConstraints.SOUTH;
		gbc_environmentDetails.fill = GridBagConstraints.HORIZONTAL;
		gbc_environmentDetails.gridx = 1;
		gbc_environmentDetails.gridy = 4;
		detailsPanelRight.add(environmentDetails, gbc_environmentDetails);
		environmentDetails.setText("                  ");
		environmentDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));

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

	public void setStartedTimeDetails(String startedTimeDetails) {
		this.startedTimeDetails.setText(startedTimeDetails);
	}

	public void setSuiteTestNameDetails(String nameDetails) {
		this.suiteTestNameDetails.setText(nameDetails);
	}

	public void setDefaultRunByDetails(String defaultRunByDetails) {
		this.defaultRunByDetails.setText(defaultRunByDetails);
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

	public void setDraftRunDetails(String draftRunDetails) {
		this.draftRunDetails.setText(draftRunDetails);
	}

	public JXLabel getLastModifiedDetails() {
		return lastModifiedDetails;
	}

	public void setLastModifiedDetails(String lastModifiedDetails) {
		this.lastModifiedDetails.setText(lastModifiedDetails);
	}

	public JXLabel getEnvironmentDetails() {
		return environmentDetails;
	}

	public void setEnvironmentDetails(String environmentDetails) {
		this.environmentDetails.setText(environmentDetails);
	}
	

}
