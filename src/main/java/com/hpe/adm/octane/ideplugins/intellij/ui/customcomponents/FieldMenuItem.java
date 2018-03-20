package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBLabel;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class FieldMenuItem extends JPanel{


    private JBCheckBox checkBox;
    private JBLabel label;

    public FieldMenuItem(String text){

        GridBagLayout gbl = new GridBagLayout();
        gbl.columnWidths = new int[]{0, 0};
        gbl.rowHeights = new int[]{0};
        gbl.columnWeights = new double[]{0.0, 1.0};
        gbl.rowWeights = new double[]{0.0, 0.0, 0.0};
        this.setLayout(gbl);

        checkBox = new JBCheckBox();
        GridBagConstraints gbcCheckBox = new GridBagConstraints();
        gbcCheckBox.anchor = GridBagConstraints.NORTHWEST;
        gbcCheckBox.gridx = 0;
        gbcCheckBox.gridy = 0;
        this.add(checkBox, gbcCheckBox);

        label = new JBLabel(text);
        label.setHorizontalAlignment(SwingConstants.LEFT);
        GridBagConstraints gbcLabel = new GridBagConstraints();
        gbcLabel.insets = new Insets(0, 10, 0, 0);
        gbcLabel.anchor = GridBagConstraints.WEST;
        gbcLabel.gridx = 1;
        gbcLabel.gridy = 0;
        this.add(label, gbcLabel);

        label.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                checkBox.doClick();
            }
        });
    }

    public void addActionListener(ActionListener actionListener){
        checkBox.addActionListener(actionListener);
    }

    public boolean isSelected(){
        return checkBox.isSelected();
    }

    public void setState(Boolean state){
        checkBox.setSelected(state);
    }

    public String getText(){
        return label.getText();
    }

}
