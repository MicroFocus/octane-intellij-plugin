package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.protobuf.ServiceException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailView;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

import java.util.ArrayList;
import java.util.List;

public class TabbedPanePresenter implements Presenter<TabbedPaneView> {

    @Inject
    TabbedPaneView tabbedPaneView;
    @Inject
    EntityService entityService;

    @Inject
    Provider<EntityDetailPresenter> entityDetailPresenterProvider;
    @Inject
    Provider<EntityTreeTablePresenter> entityTreeTablePresenterProvider;

    List<EntityDetailPresenter> detailPresenters = new ArrayList<>();
    List<EntityTreeTablePresenter> treeTablePresenters = new ArrayList<>();

    public EntityTreeTablePresenter openMyWorkTab(String tabName) {
        EntityTreeTablePresenter presenter = entityTreeTablePresenterProvider.get();
        treeTablePresenters.add(presenter);
        tabbedPaneView.addTabNoExit(tabName, presenter.getView().getComponent());
        return presenter;
    }

    public void openDetailTab(EntityModel entityModel) {
        EntityDetailPresenter presenter = entityDetailPresenterProvider.get();
        detailPresenters.add(presenter);
        presenter.setEntity(entityModel);
        tabbedPaneView.addTab(EntityDetailView.getNameForEntity(Entity.getEntityType(entityModel)) + " | " + entityModel.getValue("id").getValue().toString(), presenter.getView().getComponent());
    }

    public TabbedPaneView getView() {
        return tabbedPaneView;
    }

    @Override
    @Inject
    public void setView(TabbedPaneView tabbedPaneView) {
        this.tabbedPaneView = tabbedPaneView;

        //open test entity tree view
        EntityTreeTablePresenter presenter = openMyWorkTab(Constants.TAB_MY_WORK_TITLE);
        presenter.addEntityDoubleClickHandler((entityType, entityId, model) -> {
            //openDetailTab(model); //would still work, but it will be partially loaded in the future
            try {
                openDetailTab(entityService.findEntity(entityType, entityId));
            } catch (ServiceException ex){
                ex.printStackTrace();
            }
        });

        try {
            openDetailTab(entityService.findEntity(Entity.USER_STORY, Long.valueOf(159001)));
            openDetailTab(entityService.findEntity(Entity.TASK, Long.valueOf(52003)));
            openDetailTab(entityService.findEntity(Entity.TEST, Long.valueOf(231018)));
            openDetailTab(entityService.findEntity(Entity.DEFECT, Long.valueOf(163015)));
        } catch (ServiceException e) {
            e.printStackTrace();
        }

    }


}
