package com.hpe.adm.octane.ideplugins.intellij.ui.util;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.MultiReferenceFieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import org.apache.commons.lang.CharEncoding;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Entities;

import java.util.List;
import java.util.StringJoiner;

public class UiUtil {

    public static String getUiDataFromModel(FieldModel fieldModel) {
        return getUiDataFromModel(fieldModel, "name");
    }

    public static String getUiDataFromModel(FieldModel fieldModel, String neededProperty) {
        String result = "";
        if (null != fieldModel) {
            FieldModel tempFieldModel = null;
            if (fieldModel instanceof ReferenceFieldModel) {
                tempFieldModel = getValueOfChild((EntityModel) fieldModel.getValue(), neededProperty);
                if (null != tempFieldModel) {
                    result = String.valueOf(tempFieldModel.getValue());
                }
            } else if (fieldModel instanceof MultiReferenceFieldModel) {
                result = getValueOfChildren((List<EntityModel>) fieldModel.getValue(), neededProperty);
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

    public static FieldModel getNonNullValue(EntityModel entityModel, String... keys) {
        for (String key : keys) {
            FieldModel childModel = getValueOfChild(entityModel, key);
            if (childModel != null && childModel.getValue() != null)
                return childModel;
        }
        return null;
    }

    public static FieldModel getContainerItemForCommentModel(EntityModel commentModel) {
        return getNonNullValue(commentModel, "owner_work_item", "owner_test", "owner_run");
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

    public static String stripHtml(String html) {
        Document descriptionDoc = Jsoup.parse(html);
        descriptionDoc.outputSettings().escapeMode(Entities.EscapeMode.base);
        descriptionDoc.outputSettings().charset(CharEncoding.US_ASCII);
        descriptionDoc.outputSettings().prettyPrint(false);
        return (null == descriptionDoc.text()) ? " " : descriptionDoc.text();
    }

    public static String ellipsisTrucate(String text, int maximumLength) {
        if (text.length() > maximumLength) {
            return text.substring(0, maximumLength) + "...";
        }
        return  text;
    }

}
