package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;


import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.MetadataService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;

public class FieldEditorFactory {

    public static final String FIELD_APPMODULE = "product_areas";

    @Inject
    private MetadataService metadataService;

    public FieldEditorFactory(){

    }

    public FieldEditor createFieldEditor(EntityModelWrapper entityModelWrapper, String fieldName) {

        EntityModel entityModel = entityModelWrapper.getReadOnlyEntityModel();
        Entity entityType = entityModelWrapper.getEntityType();
        FieldMetadata fieldMetadata = metadataService.getMetadata(entityType, fieldName);

        FieldEditor fieldEditor = null;

        //EntityFieldsConstants.FIELD_APPMODULE is a wannabe tree, need to make special ui for it
        if (!fieldMetadata.isEditable() || fieldMetadata.isFinal() || fieldName.equals(FIELD_APPMODULE)) {
            fieldEditor = new ReadOnlyFieldEditor();
        } else {
            switch (fieldMetadata.getFieldType()) {
                case Integer:
                    fieldEditor = new NumericFieldEditor();
//                    ((NumericFieldEditor) fieldEditor).setBounds(0, Long.MAX_VALUE);
                    break;
                case Float:
                    fieldEditor = new NumericFieldEditor();
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
//                    try {
//                        fieldEditor = createReferenceFieldEditor(parent, entityModelWrapper, fieldMetadata);
//                    } catch (Exception e) {
//                        log.log(new Status(IStatus.ERROR, Activator.PLUGIN_ID, "Failed to create reference field editor: " + e));
                      fieldEditor = new ReadOnlyFieldEditor();
                    break;
                default:
                    fieldEditor = new ReadOnlyFieldEditor();
                    break;
            }
        }

        try {
            fieldEditor.setField(entityModelWrapper, fieldName);
        } catch (Exception ex) {
//            StringBuilder sbMessage = new StringBuilder();
//            sbMessage.append("Faied to set field ")
//                    .append(fieldName)
//                    .append(" in detail tab for entity ")
//                    .append(entityModel.getId())
//                    .append(": ")
//                    .append(ex.getMessage());
//
//            log.log(new Status(IStatus.ERROR, Activator.PLUGIN_ID, sbMessage.toString()));
            fieldEditor = new ReadOnlyFieldEditor();
            fieldEditor.setField(entityModelWrapper, fieldName);
        }
        return fieldEditor;
    }
}
