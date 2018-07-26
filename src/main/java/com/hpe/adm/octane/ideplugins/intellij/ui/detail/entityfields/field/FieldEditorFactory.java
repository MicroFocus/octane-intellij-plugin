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


import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.octane.ideplugins.services.MetadataService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;

public class FieldEditorFactory {

    private static final String FIELD_APPMODULE = "product_areas";

    @Inject
    private MetadataService metadataService;

    public FieldEditorFactory() {

    }

    public FieldEditor createFieldEditor(EntityModelWrapper entityModelWrapper, String fieldName) {
        Entity entityType = entityModelWrapper.getEntityType();
        FieldMetadata fieldMetadata = metadataService.getMetadata(entityType, fieldName);

        FieldEditor fieldEditor = null;

        //EntityFieldsConstants.FIELD_APPMODULE is a wannabe tree, need to make special ui for it
        if (!fieldMetadata.isEditable() || fieldMetadata.isFinal() || fieldName.equals(FIELD_APPMODULE)) {
            fieldEditor = new ReadOnlyFieldEditor();
        } else {
            switch (fieldMetadata.getFieldType()) {
                case Integer:
                    fieldEditor = new NumericFieldEditor(false);
                    break;
                case Float:
                    fieldEditor = new NumericFieldEditor(true);
                    break;
                case String:
                    fieldEditor = new StringFieldEditor();
                    break;
                case Boolean:
                    fieldEditor = new BooleanFieldEditor();
                    break;
                case DateTime:
                    fieldEditor = new DateTimeFieldEditor();
                    break;
                case Reference:
                    fieldEditor = new ReferenceFieldEditor();
                    break;
                default:
                    fieldEditor = new ReadOnlyFieldEditor();
                    break;
            }
        }

        try {
            fieldEditor.setField(entityModelWrapper, fieldName);
        } catch (Exception ex) {
            fieldEditor = new ReadOnlyFieldEditor();
            fieldEditor.setField(entityModelWrapper, fieldName);
        }
        return fieldEditor;
    }
}
