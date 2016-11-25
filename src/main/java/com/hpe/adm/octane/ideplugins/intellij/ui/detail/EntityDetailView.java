package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;

import javax.swing.*;

public class EntityDetailView implements View {

	private DetailsViewPanel entityModelView;

	public EntityDetailView() {
		entityModelView = new DetailsViewPanel();
	}

    public void setEntityModel(EntityModel entityModel){
		entityModelView.setTxtfldDescription((String) entityModel.getValue(DetailsViewDefaultFields.FIELD_DESCRIPTION).getValue());
		entityModelView.setTxtfldFeature((String) entityModel.getValue(DetailsViewDefaultFields.FIELD_FEATURE).getValue());
		entityModelView.setTxtfldHolder((String) entityModel.getValue(DetailsViewDefaultFields.FIELD_HOLDER).getValue());
		entityModelView.setTxtfldTeam((String) entityModel.getValue(DetailsViewDefaultFields.FIELD_TEAM).getValue());
		entityModelView.setTxtfldCustomer((String) entityModel.getValue(DetailsViewDefaultFields.FIELD_CUSTOMER).getValue());
		entityModelView.setTextfldPriority((String) entityModel.getValue(DetailsViewDefaultFields.FIELD_PRIORITY).getValue());
//		entityModelView.setTextfld((String) entityModel.getValue(DetailsViewDefaultFields.FIELD_PHASE).getValue());
		entityModelView.setTxtfldRelease((String) entityModel.getValue(DetailsViewDefaultFields.FIELD_RELEASE).getValue());
		entityModelView.setTxtfldRelease((String) entityModel.getValue(DetailsViewDefaultFields.FIELD_RELEASE).getValue());

	}

    @Override
    public JComponent getComponent() {

		return new JScrollPane(entityModelView);
	}



}
