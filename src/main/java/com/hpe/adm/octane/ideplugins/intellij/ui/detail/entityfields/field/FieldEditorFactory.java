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
