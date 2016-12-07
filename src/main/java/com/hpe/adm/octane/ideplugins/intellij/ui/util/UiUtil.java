package com.hpe.adm.octane.ideplugins.intellij.ui.util;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.MultiReferenceFieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;

import java.util.List;
import java.util.StringJoiner;

public class UiUtil {

    public static String getUiDataFromModel(FieldModel fieldModel) {
        String result = "";
        if (null != fieldModel) {
            FieldModel tempFieldModel = null;
            if (fieldModel instanceof ReferenceFieldModel) {
                tempFieldModel = getValueOfChild((EntityModel) fieldModel.getValue(), "name");
                if (null != tempFieldModel) {
                    result = String.valueOf(tempFieldModel.getValue());
                }
            } else if (fieldModel instanceof MultiReferenceFieldModel) {
                result = getValueOfChildren((List<EntityModel>) fieldModel.getValue(), "name");
            } else {
                result = String.valueOf(fieldModel.getValue());
            }
        }
        return (null == result) ? " " : result;
    }

    private static FieldModel getValueOfChild(EntityModel entityModel, String child) {
        FieldModel result = null;
        if (null != entityModel) {
            for (FieldModel fieldModel : entityModel.getValues()) {
                if (child.equals(fieldModel.getName())) {
                    result = fieldModel;
                }
            }
        }
        return result;
    }

    private static String getValueOfChildren(List<EntityModel> entityModelList, String child) {
        StringJoiner result = new StringJoiner("; ");
        String tempFieldModelValue = " ";
        if (null != entityModelList) {
            for (EntityModel entityModel : entityModelList) {
                for (FieldModel fieldModel : entityModel.getValues()) {
                    if (child.equals(fieldModel.getName())) {
                        tempFieldModelValue = String.valueOf(fieldModel.getValue());
                    }
                }
                result.add(tempFieldModelValue);
            }
        }
        return result.toString();
    }

}
