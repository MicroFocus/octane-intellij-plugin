package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;

import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;

/**
 * Created by dulaut on 1/17/2017.
 */
public class QualityStoryDetailsPanel extends StoryDetailsPanel {

    private static final long serialVersionUID = -7172388625845199450L;

    private JXLabel qualityStoryTypeDetails;

    public QualityStoryDetailsPanel() {
        JXLabel qualityStoryTypeLabel = new JXLabel();
        qualityStoryTypeLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
        qualityStoryTypeLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        qualityStoryTypeLabel.setText("Quality story type");
        GridBagConstraints gbc_lastRunsRuns = new GridBagConstraints();
        gbc_lastRunsRuns.anchor = GridBagConstraints.WEST;
        gbc_lastRunsRuns.insets = new Insets(0, 0, 5, 5);
        gbc_lastRunsRuns.gridx = 0;
        gbc_lastRunsRuns.gridy = 5;
        detailsPanelLeft.add(qualityStoryTypeLabel, gbc_lastRunsRuns);

        qualityStoryTypeDetails = new JXLabel();
        qualityStoryTypeDetails.setText(" ");
        qualityStoryTypeDetails.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_lastRunsDetails = new GridBagConstraints();
        gbc_lastRunsDetails.anchor = GridBagConstraints.SOUTH;
        gbc_lastRunsDetails.fill = GridBagConstraints.HORIZONTAL;
        gbc_lastRunsDetails.insets = new Insets(0, 0, 5, 0);
        gbc_lastRunsDetails.gridx = 1;
        gbc_lastRunsDetails.gridy = 5;
        detailsPanelLeft.add(qualityStoryTypeDetails, gbc_lastRunsDetails);
    }

    public void setQualityStoryTypeDetails(String qualityStoryTypeDetails) {
        this.qualityStoryTypeDetails.setText(qualityStoryTypeDetails);
    }
}
