package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.*;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXTextField;

public class GenericField {

    private FieldMetadata fieldMetadata;
    private boolean editable;
    private FieldModel fieldModel;


    public GenericField(FieldMetadata fieldMetadata){
        this.fieldMetadata = fieldMetadata;

        if((FieldMetadata.FieldType.Integer == fieldMetadata.getFieldType() || FieldMetadata.FieldType.Float == fieldMetadata.getFieldType()) && fieldMetadata.isEditable()){
            editable = true;
        } else {
            if(FieldMetadata.FieldType.String == fieldMetadata.getFieldType() && fieldMetadata.isEditable()){
                editable = true;
            } else {
                editable = false;
            }
        }
    }

    public boolean isEditable(){
        return editable;
    }

    public String getFieldLabel(){
        return fieldMetadata.getLabel();
    }

    public String getFieldName(){return fieldMetadata.getName();}

    public void updateField(EntityModel entityModel, String textFieldValue){
        switch (fieldMetadata.getFieldType()){
            case Integer: { fieldModel =  new LongFieldModel(fieldMetadata.getName(),Long.parseLong(textFieldValue)); break;}
            case Float: { fieldModel =  new FloatFieldModel(fieldMetadata.getName(),Float.parseFloat(textFieldValue)); break;}
            case Boolean: { fieldModel =  new BooleanFieldModel(fieldMetadata.getName(),Boolean.parseBoolean(textFieldValue)); break;}
            case String: { fieldModel = new StringFieldModel(fieldMetadata.getName(),textFieldValue); break;}
        }
        entityModel.removeValue(fieldMetadata.getName());
        entityModel.setValue(fieldModel);
    }

}
