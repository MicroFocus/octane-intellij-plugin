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

import com.hpe.adm.nga.sdk.model.BooleanFieldModel;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.EntityComboBox;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;

import java.awt.*;
import java.util.Arrays;

public class BooleanFieldEditor extends FieldEditor {
    protected EntityModelWrapper entityModelWrapper;
    protected String fieldName;

    private static String FIELD_NAME = "name";

    private EntityComboBox booleanEntityComboBox;

    private static final EntityModel ENTITY_TRUE = new EntityModel();
    private static final EntityModel ENTITY_FALSE = new EntityModel();

    static {
        ENTITY_TRUE.setValue(new StringFieldModel(FIELD_NAME, Boolean.TRUE.toString()));
        ENTITY_FALSE.setValue(new StringFieldModel(FIELD_NAME, Boolean.FALSE.toString()));
    }

    public BooleanFieldEditor() {
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[]{0, 0};
        layout.columnWeights = new double[]{0.0, 0.0};
        setLayout(layout);
        booleanEntityComboBox = new EntityComboBox();
        booleanEntityComboBox.setEntityLoader(searchQuery -> Arrays.asList(ENTITY_TRUE, ENTITY_FALSE));
        booleanEntityComboBox.setFilterable(false);
        GridBagConstraints gbc_valueTextField = new GridBagConstraints();
        gbc_valueTextField.anchor = GridBagConstraints.WEST;
        gbc_valueTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_valueTextField.gridx = 0;
        gbc_valueTextField.weightx = 1.0;
        add(booleanEntityComboBox, gbc_valueTextField);
    }

    @Override
    public void setField(EntityModelWrapper entityModel, String fieldName) {
        this.entityModelWrapper = entityModel;
        this.fieldName = fieldName;

        Boolean boolValue = (Boolean) entityModel.getValue(fieldName).getValue();
        if (boolValue) {
            booleanEntityComboBox.setSelectedEntity(ENTITY_TRUE);
        } else {
            booleanEntityComboBox.setSelectedEntity(ENTITY_FALSE);
        }
        booleanEntityComboBox.addSelectionListener(e -> {
                    boolean newValue = booleanEntityComboBox.getSelectedEntity() == ENTITY_TRUE;
                    entityModelWrapper.setValue(new BooleanFieldModel(fieldName, newValue));
                }
        );
    }
}
