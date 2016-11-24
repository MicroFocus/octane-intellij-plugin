package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;

public class EntityDetailPresenter implements Presenter<EntityDetailView> {

    EntityDetailView entityDetailView;
    EntityModel entityModel;

    public EntityDetailPresenter(){
    }

    public EntityDetailView getView(){
        return entityDetailView;
    }

    @Override
    @Inject
    public void setView(EntityDetailView entityDetailView) {
        this.entityDetailView = entityDetailView;
    }

    public void setEntity(EntityModel model){
        entityDetailView.setEntityModel(model);
    }

    public void setEntity(Long entityId){
        //use a service, do get here
        //set view
    }

}
