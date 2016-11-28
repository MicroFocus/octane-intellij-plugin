package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.MultiReferenceFieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import org.apache.commons.lang.CharEncoding;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Entities;

import javax.swing.*;
import java.util.List;

public class EntityDetailView implements View {

	private DefectsDetailsPanel entityModelView;

	public EntityDetailView() {
		entityModelView = new DefectsDetailsPanel();
	}


    public void setEntityModel(EntityModel entityModel){
		Document descriptionDoc = Jsoup.parse(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DESCRIPTION)));
		descriptionDoc.outputSettings().escapeMode(Entities.EscapeMode.base);
		descriptionDoc.outputSettings().charset(CharEncoding.US_ASCII);
		descriptionDoc.outputSettings().prettyPrint(false);

		entityModelView.setTxtfldDescription(descriptionDoc.text());
		entityModelView.setTxtfldFeature(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_FEATURE)));
		entityModelView.setTxtfldQaOwner(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_QAOWNER)));
		entityModelView.setTxtfldSeverity(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_SEVERITY)));
		entityModelView.setTxtfldTeam(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEAM)));
		entityModelView.setTextfldStoryPoints(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_STORYPOINTS)));
		entityModelView.setTextfldPriority(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PRIORITY)));
		entityModelView.setComboBoxPhase(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PHASE)));

		entityModelView.setTxtfldRelease(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_RELEASE)));
		entityModelView.setTxtfldSprint(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_SPRINT)));

		entityModelView.setTxtfldDetectedBy(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DETECTEDBY)));
		entityModelView.setTxtfldBlocked(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_BLOCKED)));
		entityModelView.setTxtfldBlockedReason(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_BLOCKEDBY)));
//		entityModelView.setTxtfldGroup(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_GROUP)));
		entityModelView.setTxtfldFeedbackType(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_FEEDBACKTYPE)));
//		entityModelView.setTxtfldEnviroment(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ENVIROMENT)));
		entityModelView.setTxtfldAppModules(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_APPMODULE)));

		entityModelView.setTextfldDetectedInPush(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DETECTEDINPPUSH)));
		entityModelView.setTxtfldDetectedInRelease(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DETECTEDINRELEASE)));
		entityModelView.setTextfldFixedInPush(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_FIXEDINPUSH)));

	}

	private String getUiDataFromModel(FieldModel fieldModel) {
		String result = "";
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

		return (null == result) ? " " : result;
	}

	private FieldModel getValueOfChild(EntityModel entityModel, String child) {
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

	private String getValueOfChildren(List<EntityModel> entityModelList, String child) {
		String result = " ";
		String tempFieldModelValue = " ";
		if (null != entityModelList) {
			for (EntityModel entityModel : entityModelList) {
				for (FieldModel fieldModel : entityModel.getValues()) {
					if (child.equals(fieldModel.getName())) {
						tempFieldModelValue = String.valueOf(fieldModel.getValue());
					}
				}
				result = result + "; " + tempFieldModelValue;
			}
		}
		return result;
	}

	@Override
	public JComponent getComponent() {
		return new JScrollPane(entityModelView);
	}


}
