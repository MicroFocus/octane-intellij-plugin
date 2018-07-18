package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import javax.swing.plaf.basic.BasicArrowButton;
import java.awt.*;

public class ReferenceFieldEditor extends FieldEditor {

    private JButton arrowButton;
    private JTextField selectionTextField;

    public ReferenceFieldEditor() {
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[]{0, 0};
        layout.columnWeights = new double[]{0.0, 0.0};
        setLayout(layout);

        selectionTextField = new JTextField();
        selectionTextField.setBackground(UIUtil.getLabelBackground());
        selectionTextField.setEditable(false);
        GridBagConstraints gbc_valueTextField = new GridBagConstraints();
        gbc_valueTextField.anchor = GridBagConstraints.WEST;
        gbc_valueTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_valueTextField.insets = new Insets(0, 0, 0, 5);
        gbc_valueTextField.gridx = 0;
        gbc_valueTextField.weightx = 1.0;
        add(selectionTextField, gbc_valueTextField);

        arrowButton = new BasicArrowButton(BasicArrowButton.SOUTH);
        GridBagConstraints gbc_arrowButton = new GridBagConstraints();
        gbc_arrowButton.anchor = GridBagConstraints.WEST;
        gbc_arrowButton.fill = GridBagConstraints.HORIZONTAL;
        gbc_arrowButton.insets = new Insets(0, 0, 0, 5);
        gbc_arrowButton.gridx = 1;
        add(arrowButton, gbc_arrowButton);
    }

    @Override
    void setField(EntityModelWrapper entityModelWrapper, String fieldName) {

    }
}
