package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

import javax.swing.*;
import java.util.List;

public class EntityDetailPresenter implements Presenter<EntityDetailView> {

    private EntityDetailView entityDetailView;
    @Inject
    private
    EntityService entityService;

    public EntityDetailPresenter() {
    }

    public EntityDetailView getView() {
        return entityDetailView;
    }

    @Override
    @Inject
    public void setView(EntityDetailView entityDetailView) {
        this.entityDetailView = entityDetailView;
    }

    public void setEntity(Entity entityType, Long entityId) {

        SwingWorker<Void, EntityModel> worker = new SwingWorker<Void, EntityModel>() {
            @Override
            protected Void doInBackground() throws Exception {
                EntityModel entityModel = null;
                try {
                    try {
                        entityModel = entityService.findEntity(entityType, entityId);
                    } catch (ServiceException e) {
                        e.printStackTrace();
                    }
                    publish(entityModel);
                    return null;
                } catch (Exception ex) {
                    throw ex;
                }
            }

            @Override
            protected void process(List<EntityModel> chunks) {
                entityDetailView.setEntityModel(chunks.get(0));
            }

        };
        worker.execute();
    }

}
