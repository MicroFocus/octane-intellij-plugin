package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.util.ui.UIUtil;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import java.awt.*;

public class ReadOnlyFieldEditor extends FieldEditor {

    private JXTextField fieldValue;

    public ReadOnlyFieldEditor() {
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[]{0, 0};
        layout.columnWeights = new double[]{0.0, 0.0};
        setLayout(layout);

        fieldValue = new JXTextField();
        fieldValue.setHorizontalAlignment(SwingConstants.LEFT);
        fieldValue.setBackground(UIUtil.getLabelBackground());
        fieldValue.setEditable(false);

        GridBagConstraints gbc_valueTextField = new GridBagConstraints();
        gbc_valueTextField.anchor = GridBagConstraints.WEST;
        gbc_valueTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_valueTextField.insets = new Insets(0, 0, 0, 5);
        gbc_valueTextField.gridx = 0;
        gbc_valueTextField.weightx = 1.0;
        add(fieldValue, gbc_valueTextField);
    }

    @Override
    public void setField(EntityModelWrapper entityModelWrapper, String fieldName) {
        String fieldVal = Util.getUiDataFromModel(entityModelWrapper.getValue(fieldName));
        fieldValue.setText(fieldVal);
        fieldValue.setToolTipText(fieldVal);
        //Removes a bunch of unnecessary listeners
        if (fieldValue.getText().isEmpty()) {
            fieldValue.setEnabled(false);
        } else {
            fieldValue.setEnabled(true);
        }
    }
}
