/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors (“Open Text”) are as may be set forth
 * in the express warranty statements accompanying such products and services.
 * Nothing herein should be construed as constituting an additional warranty.
 * Open Text shall not be liable for technical or editorial errors or
 * omissions contained herein. The information contained herein is subject
 * to change without notice.
 *
 * Except as specifically indicated otherwise, this document contains
 * confidential information and a valid license is required for possession,
 * use or copying. If this work is provided to the U.S. Government,
 * consistent with FAR 12.211 and 12.212, Commercial Computer Software,
 * Computer Software Documentation, and Technical Data for Commercial Items are
 * licensed to the U.S. Government under vendor's standard commercial license.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.nga.sdk.model.ReferenceErrorModel;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ui.JBColor;
import com.intellij.ui.RoundedLineBorder;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;

public class StringFieldEditor extends FieldEditor {

    private String fieldName;
    private JXTextField fieldValue;
    private EntityModelWrapper entityModelWrapper;
    private DocumentListener documentListener;

    public StringFieldEditor() {

        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[]{0, 0};
        layout.columnWeights = new double[]{0.0, 0.0};
        setLayout(layout);

        setBorder(new RoundedLineBorder(JBColor.GRAY, 5));

        fieldValue = new JXTextField();
        fieldValue.setBorder(BorderFactory.createEmptyBorder(2, 5, 2, 2));
        GridBagConstraints gbc_valueTextField = new GridBagConstraints();
        gbc_valueTextField.anchor = GridBagConstraints.WEST;
        gbc_valueTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_valueTextField.gridx = 0;
        gbc_valueTextField.weightx = 1.0;
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

    private void handleTextChange() {
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
        String fieldVal = Util.getUiDataFromModel(entityModelWrapper.getValue(fieldName));
        fieldValue.setText(fieldVal);
        fieldValue.setToolTipText(fieldVal);
        fieldValue.getDocument().addDocumentListener(documentListener);
    }
}
