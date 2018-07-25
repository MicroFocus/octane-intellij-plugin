package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.util.ui.GridBag;
import com.intellij.util.ui.UIUtil;
import net.miginfocom.layout.Grid;

import javax.swing.*;
import java.awt.*;

public class EntityComboBox extends JPanel {

    private JLabel arrowButton;
    private JTextField editorLabel;
    private JSeparator separator;

    public EntityComboBox() {
        GridBagLayout gbl = new GridBagLayout();
        gbl.columnWidths = new int[]{0, 0};
        gbl.columnWeights = new double[]{0.0, 0.0};
        setLayout(gbl);

        setBorder(BorderFactory.createLineBorder(Color.GRAY, 1, true));

        editorLabel = new JTextField("mockLabel");
        editorLabel.setEditable(false);
        editorLabel.setBorder(null);
        GridBagConstraints gbc_editorLabel = new GridBagConstraints();
        gbc_editorLabel.anchor = GridBagConstraints.WEST;
        gbc_editorLabel.fill = GridBagConstraints.HORIZONTAL;
        gbc_editorLabel.insets = new Insets(0,5,0,0);
        gbc_editorLabel.gridx = 0;
        gbc_editorLabel.weightx = 1.0;
        add(editorLabel, gbc_editorLabel);

        separator = new JSeparator(SwingConstants.VERTICAL);
        GridBagConstraints gbc_separator = new GridBagConstraints();
        gbc_separator.gridx = 1;
        gbc_separator.fill = GridBagConstraints.VERTICAL;
        add(separator, gbc_separator);

        arrowButton = new JLabel(IconLoader.findIcon(Constants.IMG_ENTITY_COMBOBOX_ARROW));
        GridBagConstraints gbc_arrowButton = new GridBagConstraints();
        gbc_arrowButton.anchor = GridBagConstraints.WEST;
        gbc_arrowButton.gridx = 2;
        add(arrowButton, gbc_arrowButton);
    }
}
