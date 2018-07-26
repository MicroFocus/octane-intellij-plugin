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

import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.EntityComboBox;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.intellij.ide.ui.laf.darcula.ui.DarculaComboBoxUI;
import com.intellij.openapi.ui.ComboBoxWithWidePopup;
import com.intellij.openapi.ui.JBPopupMenu;
import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import javax.swing.plaf.basic.BasicArrowButton;
import javax.swing.plaf.basic.BasicComboBoxUI;
import java.awt.*;

public class ReferenceFieldEditor extends FieldEditor {

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
        gbc_valueTextField.weightx = 0.5;
        //add(selectionTextField, gbc_valueTextField);


        GridBagConstraints gbc_arrowButton = new GridBagConstraints();
        gbc_arrowButton.anchor = GridBagConstraints.WEST;
        gbc_arrowButton.fill = GridBagConstraints.HORIZONTAL;
        gbc_arrowButton.insets = new Insets(0, 5, 0, 10);
        gbc_arrowButton.gridx = 1;
        gbc_arrowButton.weightx = 1.0;
        EntityComboBox ebb = new EntityComboBox();
        add(ebb, gbc_arrowButton);
    }

    @Override
    void setField(EntityModelWrapper entityModelWrapper, String fieldName) {

    }
}
