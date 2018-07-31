/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.nga.sdk.model.FloatFieldModel;
import com.hpe.adm.nga.sdk.model.LongFieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ui.RoundedLineBorder;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

public class NumericFieldEditor extends FieldEditor {
    private EntityModelWrapper entityModelWrapper;
    private String fieldName;
    private JXTextField textField;

    private DocumentListener modifyListener;
    private boolean isRealNumber;

    public NumericFieldEditor(boolean isRealNumber) {
        this.isRealNumber = isRealNumber;
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[]{0, 0};
        layout.columnWeights = new double[]{0.0, 0.0};
        setLayout(layout);

        setBorder(new RoundedLineBorder(Color.GRAY, 5));

        textField = new JXTextField();
        textField.setBorder(BorderFactory.createEmptyBorder(2, 5, 2, 2));
        textField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyTyped(KeyEvent e) {
                if (isRealNumber) {
                    if ((e.getKeyChar() < '0' || e.getKeyChar() > '9') && e.getKeyChar() != '.') {
                        e.consume();
                    }
                    if (textField.getText().contains(".")) {
                        e.consume();
                    }
                } else {
                    if (e.getKeyChar() < '0' || e.getKeyChar() > '9') {
                        e.consume();
                    }
                }
            }
        });
        GridBagConstraints gbc_valueTextField = new GridBagConstraints();
        gbc_valueTextField.anchor = GridBagConstraints.WEST;
        gbc_valueTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_valueTextField.insets = new Insets(0, 0, 0, 0);
        gbc_valueTextField.gridx = 0;
        gbc_valueTextField.weightx = 1.0;
        add(textField, gbc_valueTextField);

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


    private void handleValueChange() {
        if (textField.getText().isEmpty()) {
            entityModelWrapper.setValue(new ReferenceFieldModel(fieldName, null));
        } else {
            if (isRealNumber) {
                try {
                    Float value = Float.parseFloat(textField.getText());
                    entityModelWrapper.setValue(new FloatFieldModel(fieldName, value));
                    textField.setForeground(getForeground());
                } catch (Exception ignored) {
                    textField.setForeground(Color.RED);
                }
            } else {
                try {
                    Long value = Long.parseLong(textField.getText());
                    entityModelWrapper.setValue(new LongFieldModel(fieldName, value));
                    textField.setForeground(getForeground());
                } catch (Exception ignored) {
                    textField.setForeground(Color.RED);
                }
            }

        }
    }

    public void setField(EntityModelWrapper entityModel, String fieldName) {
        this.entityModelWrapper = entityModel;
        this.fieldName = fieldName;
        textField.getDocument().removeDocumentListener(modifyListener);
        textField.setText(Util.getUiDataFromModel(entityModel.getValue(fieldName)));
        textField.getDocument().addDocumentListener(modifyListener);
    }
}
