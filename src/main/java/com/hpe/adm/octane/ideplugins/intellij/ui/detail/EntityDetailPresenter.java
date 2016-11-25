package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.google.inject.Inject;
import com.google.protobuf.ServiceException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

public class EntityDetailPresenter implements Presenter<EntityDetailView> {

    EntityDetailView entityDetailView;
    EntityModel entityModel;
    @Inject
    EntityService entityService;

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
        EntityModel entityModel = null;
        try {
            entityModel = entityService.findEntity(Entity.DEFECT, 1033);//24068
        } catch (ServiceException e) {
            e.printStackTrace();
        }
        this.setEntity(entityModel);

    }

}
