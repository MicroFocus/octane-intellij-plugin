package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.intellij.util.RestUtil;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceRuntimeException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

import java.util.Collection;

public class EntityDetailPresenter implements Presenter<EntityDetailView> {

    private EntityDetailView entityDetailView;
    @Inject
    private EntityService entityService;
    private Entity entityType;
    private Long entityId;


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
        this.entityType = entityType;
        this.entityId = entityId;

        RestUtil.runInBackground(
                () -> {
                    try {
                        return entityService.findEntity(entityType, entityId);
                    } catch (ServiceException ex) {
                        entityDetailView.setErrorMessage(ex.getMessage());
                        throw new ServiceRuntimeException(ex.getMessage());
                    }
                },
                (entityModel) -> {
                    entityDetailView.setEntityModel(entityModel);
                    entityDetailView.setRefreshEntityButton(new EntityRefreshAction());
                    entityDetailView.setPossiblePhasesForEntity(getNextPhase(entityModel));
                },
                null,
                "Failed to fetch entity: " + entityType.name() + ": " + entityId,
                "Loading entity " + entityType.name() + ": " + entityId);
    }

    private Collection<EntityModel> getNextPhase(EntityModel entityModel) {
        Long currentPhaseId = Long.valueOf(UiUtil.getUiDataFromModel(entityModel.getValue("phase"), "id"));
        return entityService.findPossibleTransitionFromCurrentPhase(Entity.getEntityType(entityModel), currentPhaseId);

    }

    private final class EntityRefreshAction extends AnAction {
        public EntityRefreshAction() {
            super("Refresh current entity", "this will refresh the current entity", IconLoader.findIcon(Constants.IMG_REFRESH_ICON));
        }

        public void actionPerformed(AnActionEvent e) {
            entityDetailView.doRefresh();
            setEntity(entityType, entityId);

        }
    }

}
