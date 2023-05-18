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

import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.MultiReferenceFieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.EntityComboBox;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.intellij.openapi.util.IconLoader;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collections;

public class ReferenceFieldEditor extends FieldEditor {

    private JLabel clearSelection;
    private EntityComboBox entityComboBox;

    private EntityModelWrapper entityModelWrapper;
    private String fieldName;
    private MouseAdapter mouseListener = new MouseAdapter() {
        @Override
        public void mouseClicked(MouseEvent e) {
            if (entityComboBox.isMultiSelect()) {
                entityModelWrapper.setValue(new MultiReferenceFieldModel(fieldName, Collections.emptyList()));
            } else {
                entityModelWrapper.setValue(new ReferenceFieldModel(fieldName, null));
            }
            entityComboBox.clearEditor();
            disableArrowButton();
        }
    };

    public ReferenceFieldEditor() {
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[]{0, 0};
        layout.columnWeights = new double[]{0.0, 0.0};
        setLayout(layout);

        entityComboBox = new EntityComboBox();
        entityComboBox.addSelectionListener(e -> {
            if (entityComboBox.isMultiSelect()) {
                entityModelWrapper.setValue(new MultiReferenceFieldModel(fieldName, entityComboBox.getSelectedEntities()));
                if (entityComboBox.getSelectedEntities().size() == 0) {
                    disableArrowButton();
                } else {
                    enableArrowButton();
                }
            } else {
                entityModelWrapper.setValue(new ReferenceFieldModel(fieldName, entityComboBox.getSelectedEntity()));
                enableArrowButton();
            }
        });
        GridBagConstraints gbc_entityComboBox = new GridBagConstraints();
        gbc_entityComboBox.anchor = GridBagConstraints.WEST;
        gbc_entityComboBox.fill = GridBagConstraints.HORIZONTAL;
        gbc_entityComboBox.gridx = 0;
        gbc_entityComboBox.weightx = 1.0;
        add(entityComboBox, gbc_entityComboBox);

        clearSelection = new JLabel();
        clearSelection.setCursor(new Cursor(Cursor.HAND_CURSOR));
        clearSelection.setIcon(IconLoader.findIcon(Constants.getOctaneRemoveIcon(), ReferenceFieldEditor.class.getClassLoader()));
        clearSelection.addMouseListener(mouseListener);
    }

    @Override
    public void setField(EntityModelWrapper entityModelWrapper, String fieldName) {
        this.entityModelWrapper = entityModelWrapper;
        this.fieldName = fieldName;

        FieldModel fieldModel = entityModelWrapper.getValue(fieldName);

        boolean hasValue = fieldModel != null && fieldModel.getValue() != null;

        //Additional check for MultiReferenceFieldModel
        if (hasValue && fieldModel instanceof MultiReferenceFieldModel) {
            hasValue = !((MultiReferenceFieldModel) fieldModel).getValue().isEmpty();
        }

        if (hasValue) {
            if (fieldModel instanceof ReferenceFieldModel && !entityComboBox.isMultiSelect()) {
                entityComboBox.setSelectedEntity(((ReferenceFieldModel) fieldModel).getValue());
            } else if (fieldModel instanceof MultiReferenceFieldModel && entityComboBox.isMultiSelect()) {
                entityComboBox.setSelectedEntities(((MultiReferenceFieldModel) fieldModel).getValue());
            } else {
                throw new RuntimeException("Failed to set value of the Reference field model, field value and metadata not compatible");
            }
            clearSelection.setIcon(IconLoader.findIcon(Constants.getOctaneRemoveIcon(), ReferenceFieldEditor.class.getClassLoader()));
            clearSelection.setCursor(new Cursor(Cursor.HAND_CURSOR));
            clearSelection.addMouseListener(mouseListener);
        } else {
            disableArrowButton();
            entityComboBox.clearEditor();
        }
    }

    public void setEntityLoader(EntityComboBox.EntityLoader entityLoader) {
        entityComboBox.setEntityLoader(entityLoader);
    }

    public void setMultiSelect(boolean multiSelect) {
        entityComboBox.setMultiSelect(multiSelect);
    }

    public Component getClearButton() {
        return clearSelection;
    }

    private void disableArrowButton() {
        clearSelection.setIcon(IconLoader.findIcon(Constants.getOctaneRemoveDisabledIcon(), ReferenceFieldEditor.class.getClassLoader()));
        clearSelection.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        clearSelection.removeMouseListener(mouseListener);
    }

    private void enableArrowButton() {
        clearSelection.setIcon(IconLoader.findIcon(Constants.getOctaneRemoveIcon(), ReferenceFieldEditor.class.getClassLoader()));
        clearSelection.setCursor(new Cursor(Cursor.HAND_CURSOR));
        clearSelection.addMouseListener(mouseListener);
    }

}
