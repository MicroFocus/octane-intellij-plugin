package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXTextField;

public class GenericField {

    private FieldMetadata fieldMetadata;
    private EntityService entityService;
    private EntityModel entityModel;
    private boolean editable;

    public GenericField(FieldMetadata fieldMetadata, EntityService entityService, EntityModel entityModel){
        this.fieldMetadata = fieldMetadata;
        this.entityService = entityService;
        this.entityModel = entityModel;

        if("Reference".equals(fieldMetadata.getFieldType())){
            editable = false;
        } else {
            editable = true;
        }
    }

    public boolean isEditable(){
        return editable;
    }

    public String getFieldLabel(){
        return fieldMetadata.getLabel();
    }

    public String getFieldName(){return fieldMetadata.getName();}

    public void updateField(){
        //TODO after common implements the method call it here
    }

}
