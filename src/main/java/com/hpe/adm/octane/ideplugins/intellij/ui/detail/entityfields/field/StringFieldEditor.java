package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.nga.sdk.model.ReferenceErrorModel;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;

public class StringFieldEditor extends JPanel implements FieldEditor {

    private String fieldName;
    private JTextField fieldValue;
    private EntityModelWrapper entityModelWrapper;
    private DocumentListener documentListener;

    public StringFieldEditor(){

        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[] { 0, 0};
        layout.columnWeights = new double[] { 0.0, 0.0};
        setLayout(layout);

        fieldValue = new JTextField();
        fieldValue.setColumns(20);
        GridBagConstraints gbc_valueTextField = new GridBagConstraints();
        gbc_valueTextField.anchor = GridBagConstraints.WEST;
        gbc_valueTextField.insets = new Insets(0, 0, 0, 5);
        gbc_valueTextField.gridx = 0;
        add(fieldValue, gbc_valueTextField);

        documentListener = new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                handleTextChange();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                handleTextChange();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                handleTextChange();
            }
        };

    }

    public void handleTextChange(){
        String text = fieldValue.getText();
        // whitespace is considered null
        if (text.trim().isEmpty()) {
            entityModelWrapper.setValue(new ReferenceErrorModel(fieldName, null));
        } else {
            entityModelWrapper.setValue(new StringFieldModel(fieldName, text));
        }
    }

    @Override
    public void setField(EntityModelWrapper entityModelWrapper, String fieldName) {
        this.fieldName = fieldName;
        this.entityModelWrapper = entityModelWrapper;
        fieldValue.getDocument().removeDocumentListener(documentListener);
        fieldValue.setText(Util.getUiDataFromModel(entityModelWrapper.getValue(fieldName)));
        fieldValue.getDocument().addDocumentListener(documentListener);
    }
}
