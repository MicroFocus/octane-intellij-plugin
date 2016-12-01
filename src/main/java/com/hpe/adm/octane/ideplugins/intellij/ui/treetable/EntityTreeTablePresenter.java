package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.services.EntityService;

import java.util.Collection;

public class EntityTreeTablePresenter implements Presenter<EntityTreeView>{

    EntityTreeView entityTreeTableView;
    EntityTreeModel entityTreeModel = new EntityTreeModel();

    @Inject
    EntityService entityService;

    public EntityTreeTablePresenter(){
    }

    public void refresh(){

        Collection<EntityModel> myWork = entityService.getMyWork();
        entityTreeModel.setEntities(myWork);
        entityTreeTableView.setTreeModel(entityTreeModel);

        /*
        //Async get
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
            @Override
            protected Void doInBackground() throws Exception {
                entityTreeTableView.setLoading(true);
                EntityTreeModel model = new EntityTreeModel(entityService.findEntities(Entity.WORK_ITEM));
                entityTreeTableView.setTreeModel(model);
                return null;
            }
            @Override
            protected void done(){
                entityTreeTableView.setLoading(false);
            }
        };
        worker.execute();
        */
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

}
