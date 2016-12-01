package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.services.EntityService;

import javax.swing.*;
import java.util.Collection;

public class EntityTreeTablePresenter implements Presenter<EntityTreeView>{

    EntityTreeView entityTreeTableView;
    EntityTreeModel entityTreeModel = new EntityTreeModel();

    @Inject
    EntityService entityService;

    public EntityTreeTablePresenter(){
    }

    public void refresh(){

//        Collection<EntityModel> myWork = entityService.getMyWork();
//        entityTreeModel.setEntities(myWork);
//        entityTreeTableView.setTreeModel(entityTreeModel);

        //Async get
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
            @Override
            protected Void doInBackground() throws Exception {
                try {
                    entityTreeTableView.setLoading(true);
                    Collection<EntityModel> myWork = entityService.getMyWork();
                    entityTreeModel.setEntities(myWork);
                    SwingUtilities.invokeLater(() -> entityTreeTableView.setTreeModel(entityTreeModel));
                    return null;
                } catch (Exception ex){
                    System.out.println(ex);
                    throw ex;
                }
            }
            @Override
            protected void done(){
                entityTreeTableView.setLoading(false);
            }
        };
        worker.execute();

    }

    public EntityTreeView getView(){
        return entityTreeTableView;
    }

    @Override
    @Inject
    public void setView(EntityTreeView entityTreeView) {
        this.entityTreeTableView = entityTreeView;

        //start presenting
        entityTreeTableView.addRefreshButtonActionListener(event ->  refresh());
        refresh();
    }

    public void addEntityDoubleClickHandler(EntityTreeView.EntityDoubleClickHandler handler) {
        getView().addEntityDoubleClickHandler(handler);
    }
}
