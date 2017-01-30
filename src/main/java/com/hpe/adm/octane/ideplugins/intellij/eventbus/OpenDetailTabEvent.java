package com.hpe.adm.octane.ideplugins.intellij.eventbus;

import com.hpe.adm.nga.sdk.model.EntityModel;

public class OpenDetailTabEvent {

    private EntityModel entityModel;

    public OpenDetailTabEvent(EntityModel entityModel) {
        this.entityModel = entityModel;
    }

    public EntityModel getEntityModel() {
        return entityModel;
    }

}