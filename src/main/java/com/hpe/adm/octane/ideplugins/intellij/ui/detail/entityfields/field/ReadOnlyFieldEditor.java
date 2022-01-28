/*
 * © Copyright 2017-2022 Micro Focus or one of its affiliates.
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

import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ui.JBColor;
import com.intellij.ui.RoundedLineBorder;
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

        setBorder(new RoundedLineBorder(JBColor.GRAY, 5));

        fieldValue = new JXTextField();
        fieldValue.setBackground(UIUtil.getLabelBackground());
        fieldValue.setEditable(false);
        fieldValue.setBorder(BorderFactory.createEmptyBorder(2, 5, 2, 2));

        GridBagConstraints gbc_valueTextField = new GridBagConstraints();
        gbc_valueTextField.anchor = GridBagConstraints.WEST;
        gbc_valueTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_valueTextField.gridx = 0;
        gbc_valueTextField.weightx = 1.0;
        add(fieldValue, gbc_valueTextField);
    }

    @Override
    public void setField(EntityModelWrapper entityModelWrapper, String fieldName) {
        String fieldVal = Util.getUiDataFromModel(entityModelWrapper.getValue(fieldName));
        fieldValue.setText(fieldVal);
        fieldValue.setToolTipText(fieldVal);

        if (fieldValue.getText().isEmpty()) {
            fieldValue.setEnabled(false);
        } else {
            fieldValue.setEnabled(true);
        }
    }
}
