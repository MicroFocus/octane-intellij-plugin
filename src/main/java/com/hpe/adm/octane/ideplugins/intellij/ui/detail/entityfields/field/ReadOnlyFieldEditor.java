package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;

import javax.swing.*;
import java.awt.*;

public class ReadOnlyFieldEditor extends JPanel implements FieldEditor {

    private JLabel fieldValue;

    public ReadOnlyFieldEditor(){
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[] { 0, 0};
        layout.columnWeights = new double[] { 0.0, 0.0};
        setLayout(layout);

        fieldValue = new JLabel();
        GridBagConstraints gbc_valueTextField = new GridBagConstraints();
        gbc_valueTextField.anchor = GridBagConstraints.WEST;
        gbc_valueTextField.insets = new Insets(0, 0, 0, 5);
        gbc_valueTextField.gridx = 0;
        add(fieldValue, gbc_valueTextField);
    }

    @Override
    public void setField(EntityModelWrapper entityModelWrapper, String fieldName) {
        fieldValue.setText(Util.getUiDataFromModel(entityModelWrapper.getValue(fieldName)));

        //Removes a bunch of unnecessary listeners
        if(fieldValue.getText().isEmpty()) {
            fieldValue.setEnabled(false);
        } else {
            fieldValue.setEnabled(true);
        }
    }
}
