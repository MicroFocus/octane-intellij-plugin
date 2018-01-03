package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entity;

/*
 * © 2018 EntIT Software LLC, a Micro Focus company, L.P.
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

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

public class RequirementsDetailsPanel extends JXPanel {
    private static final long serialVersionUID = -7172388625845199450L;
    private JXLabel ownerDetails;
    private JXLabel authorDetails;
    private JXLabel releaseDetails;
    private JXLabel lastModifiedDetails;
    private JXLabel creationTimeDetails;


    public RequirementsDetailsPanel() {
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
        gbl_detailsPanelLeft.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelLeft.columnWeights = new double[]{0.0, 1.0};
        gbl_detailsPanelLeft.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelLeft.setLayout(gbl_detailsPanelLeft);


        JXLabel releaseLabel = new JXLabel();
        releaseLabel.setText("Release");
        releaseLabel.setFont(new Font("Arial", Font.BOLD, 12));
        releaseLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_releaseLabel = new GridBagConstraints();
        gbc_releaseLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_releaseLabel.insets = new Insets(0, 0, 5, 5);
        gbc_releaseLabel.gridx = 0;
        gbc_releaseLabel.gridy = 6;
        detailsPanelLeft.add(releaseLabel, gbc_releaseLabel);

        releaseDetails = new JXLabel();
        releaseDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        releaseDetails.setText("        ");
        releaseDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_releaseDetails = new GridBagConstraints();
        gbc_releaseDetails.anchor = GridBagConstraints.SOUTH;
        gbc_releaseDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_releaseDetails.insets = new Insets(0, 0, 5, 0);
        gbc_releaseDetails.gridx = 1;
        gbc_releaseDetails.gridy = 6;
        detailsPanelLeft.add(releaseDetails, gbc_releaseDetails);


        JXLabel lastModifiedLabel = new JXLabel();
        lastModifiedLabel.setText("Last modified");
        lastModifiedLabel.setFont(new Font("Arial", Font.BOLD, 12));
        lastModifiedLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lastModifiedLabel = new GridBagConstraints();
        gbc_lastModifiedLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_lastModifiedLabel.insets = new Insets(0, 0, 0, 5);
        gbc_lastModifiedLabel.gridx = 0;
        gbc_lastModifiedLabel.gridy = 8;
        detailsPanelLeft.add(lastModifiedLabel, gbc_lastModifiedLabel);

        lastModifiedDetails = new JXLabel();
        lastModifiedDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        lastModifiedDetails.setText("        ");
        lastModifiedDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_lastModifiedDetails = new GridBagConstraints();
        gbc_lastModifiedDetails.anchor = GridBagConstraints.SOUTH;
        gbc_lastModifiedDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_lastModifiedDetails.gridx = 1;
        gbc_lastModifiedDetails.gridy = 8;
        detailsPanelLeft.add(lastModifiedDetails, gbc_lastModifiedDetails);

        JXLabel creationTimeLabel = new JXLabel();
        creationTimeLabel.setText("Creation time");
        creationTimeLabel.setFont(new Font("Arial", Font.BOLD, 12));
        creationTimeLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_creationTimeLabel = new GridBagConstraints();
        gbc_creationTimeLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_creationTimeLabel.insets = new Insets(0, 0, 5, 5);
        gbc_creationTimeLabel.gridx = 0;
        gbc_creationTimeLabel.gridy = 7;
        detailsPanelLeft.add(creationTimeLabel, gbc_creationTimeLabel);

        creationTimeDetails = new JXLabel();
        creationTimeDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        creationTimeDetails.setText("        ");
        creationTimeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_creationTimeDetails = new GridBagConstraints();
        gbc_creationTimeDetails.anchor = GridBagConstraints.SOUTH;
        gbc_creationTimeDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_creationTimeDetails.insets = new Insets(0, 0, 5, 0);
        gbc_creationTimeDetails.gridx = 1;
        gbc_creationTimeDetails.gridy = 7;
        detailsPanelLeft.add(creationTimeDetails, gbc_creationTimeDetails);

        JXPanel detailsPanelRight = new JXPanel();
        detailsPanelRight.setBorder(null);
        detailsPanelMain.add(detailsPanelRight, BorderLayout.CENTER);
        GridBagLayout gbl_detailsPanelRight = new GridBagLayout();
        gbl_detailsPanelRight.columnWidths = new int[]{0, 0, 0};
        gbl_detailsPanelRight.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelRight.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gbl_detailsPanelRight.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelRight.setLayout(gbl_detailsPanelRight);

        JXLabel ownerLabel = new JXLabel();
        ownerLabel.setFont(new Font("Arial", Font.BOLD, 12));
        GridBagConstraints gbc_ownerLabel = new GridBagConstraints();
        gbc_ownerLabel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_ownerLabel.insets = new Insets(0, 0, 5, 5);
        gbc_ownerLabel.gridx = 0;
        gbc_ownerLabel.gridy = 0;
        detailsPanelRight.add(ownerLabel, gbc_ownerLabel);
        ownerLabel.setBorder(new EmptyBorder(0, 30, 0, 10));
        ownerLabel.setText("Owner");

        ownerDetails = new JXLabel();
        ownerDetails.setFont(new Font("Arial", Font.PLAIN, 12));
        GridBagConstraints gbc_ownerDetails = new GridBagConstraints();
        gbc_ownerDetails.anchor = GridBagConstraints.SOUTH;
        gbc_ownerDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_ownerDetails.insets = new Insets(0, 0, 5, 0);
        gbc_ownerDetails.gridx = 1;
        gbc_ownerDetails.gridy = 0;
        detailsPanelRight.add(ownerDetails, gbc_ownerDetails);
        ownerDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        ownerDetails.setText("        ");

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
        authorDetails.setText("               ");
        authorDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));

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

    public void setReleaseDetails(String releaseDetails) {
        this.releaseDetails.setText(releaseDetails);
    }

    public void setLastModifiedDetails(String lastModifiedDetails) {
        this.lastModifiedDetails.setText(lastModifiedDetails);
    }
    public void setCreationTimeDetails(String creationTimeDetails) {
        this.creationTimeDetails.setText(creationTimeDetails);
    }
    public void setAuthorDetails(String authorDetails){this.authorDetails.setText(authorDetails);}


}
