package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.nga.sdk.model.FloatFieldModel;
import com.hpe.adm.nga.sdk.model.LongFieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.NumberFormatter;
import java.awt.*;
import java.text.NumberFormat;

public class NumericFieldEditor extends JPanel implements FieldEditor {
    private EntityModelWrapper entityModelWrapper;
    private String fieldName;
    private JFormattedTextField textField;

    private DocumentListener modifyListener;
    private boolean isRealNumber;

    public NumericFieldEditor(boolean isRealNumber) {
        this.isRealNumber = isRealNumber;
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[] { 0, 0};
        layout.columnWeights = new double[] { 0.0, 0.0};
        setLayout(layout);

        NumberFormat format = NumberFormat.getInstance();
        NumberFormatter formatter = new NumberFormatter(format);
        if(isRealNumber){
            formatter.setValueClass(Float.class);
        } else {
            formatter.setValueClass(Integer.class);
        }
        formatter.setAllowsInvalid(false);
        formatter.setCommitsOnValidEdit(true);

        textField = new JFormattedTextField(formatter);
        textField.setColumns(20);

        modifyListener = new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                handleValueChange();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                handleValueChange();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                handleValueChange();
            }
        };
    }


    private void handleValueChange(){
        if (textField.getText().isEmpty()) {
            entityModelWrapper.setValue(new ReferenceFieldModel(fieldName, null));
        } else {
            if (isRealNumber) {
                try {
                    Float value = Float.parseFloat(textField.getText());
                    entityModelWrapper.setValue(new FloatFieldModel(fieldName, value));
                } catch (Exception ignored) {
                }
            } else {
                try {
                    Long value = Long.parseLong(textField.getText());
                    entityModelWrapper.setValue(new LongFieldModel(fieldName, value));
                } catch (Exception ignored) {
                }
            }

        }
    }

    @Override
    public void setField(EntityModelWrapper entityModel, String fieldName) {
        this.entityModelWrapper = entityModel;
        this.fieldName = fieldName;
        textField.getDocument().removeDocumentListener(modifyListener);
        textField.setText(Util.getUiDataFromModel(entityModel.getValue(fieldName)));
        textField.getDocument().addDocumentListener(modifyListener);
    }
}
