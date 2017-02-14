package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork.NoWorkPanel;
import com.hpe.adm.octane.services.util.Constants;

import javax.swing.*;
import java.awt.*;

/**
 * Shown when no search result are found
 */
public class NoSearchResultsPanel extends JPanel {

    public NoSearchResultsPanel(){
        GridBagLayout gbl_panelNoWork = new GridBagLayout();
        gbl_panelNoWork.columnWidths = new int[]{364, 0};
        gbl_panelNoWork.rowHeights = new int[]{0, 14, 0};
        gbl_panelNoWork.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_panelNoWork.rowWeights = new double[]{1.0, 1.0, Double.MIN_VALUE};
        setLayout(gbl_panelNoWork);

        JLabel label = new JLabel("");
        label.setIcon(new ImageIcon(NoWorkPanel.class.getResource(Constants.IMG_UNIDRAG_SMALL_SAD)));
        GridBagConstraints gbc_label = new GridBagConstraints();
        gbc_label.anchor = GridBagConstraints.SOUTH;
        gbc_label.insets = new Insets(0, 0, 5, 0);
        gbc_label.gridx = 0;
        gbc_label.gridy = 0;
        add(label, gbc_label);

        JLabel lblCongratulationsForFinishing = new JLabel("No results");
        GridBagConstraints gbc_lblCongratulationsForFinishing = new GridBagConstraints();
        gbc_lblCongratulationsForFinishing.anchor = GridBagConstraints.NORTH;
        gbc_lblCongratulationsForFinishing.insets = new Insets(0, 0, 5, 0);
        gbc_lblCongratulationsForFinishing.gridx = 0;
        gbc_lblCongratulationsForFinishing.gridy = 1;
        add(lblCongratulationsForFinishing, gbc_lblCongratulationsForFinishing);
    }
}
