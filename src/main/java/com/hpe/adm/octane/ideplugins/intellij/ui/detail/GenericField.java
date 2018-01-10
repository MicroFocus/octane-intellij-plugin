package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.EntityService;

public class GenericField {

    private FieldMetadata fieldMetadata;
    private EntityService entityService;
    private EntityModel entityModel;
    private boolean editable;

    public GenericField(FieldMetadata fieldMetadata, EntityService entityService, EntityModel entityModel){

        this.fieldMetadata = fieldMetadata;
        this.entityService = entityService;
        this.entityModel = entityModel;

        if("string".equals(fieldMetadata.getFieldType()) || "integer".equals(fieldMetadata.getFieldType())){
            editable = true;
        } else {
            editable = false;
        }

    }

    public boolean isEditable(){
        return editable;
    }

    public String getFieldLabel(String name){
        return fieldMetadata.getLabel();
    }

    public void updateField(){
        //TODO after common implements the method call it here
    }

}
