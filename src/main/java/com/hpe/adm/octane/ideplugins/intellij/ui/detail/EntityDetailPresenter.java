package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;

public class EntityDetailPresenter {

    EntityDetailView entityDetailView;
    EntityModel entityModel;

    public EntityDetailPresenter(){
        entityDetailView = new EntityDetailView(this);
    }

    public HasComponent getView(){
        return entityDetailView;
    }

    public void setEntity(EntityModel model){
        entityDetailView.setEntityModel(model);
    }

    public void setEntity(Long entityId){
        //use a service, do get here
        //set view
    }

}
