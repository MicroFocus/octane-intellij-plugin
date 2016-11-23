package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

import javax.swing.*;

/**
 * Created by tothan on 11/22/2016.
 */
public class EntityTreeTablePresenter implements Presenter<EntityTreeTableView>{

    EntityTreeTableModel treeTableModel = new EntityTreeTableModel();
    EntityTreeTableView entityTreeTableView;

    @Inject
    EntityService entityService;

    public EntityTreeTablePresenter(){
    }

    public void refresh(){
        //Async get
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
            @Override
            protected Void doInBackground() throws Exception {
                entityTreeTableView.setLoading(true);
                treeTableModel.setEntities(entityService.findEntities(Entity.WORK_ITEM), "subtype");
                return null;
            }
            @Override
            protected void done(){
                entityTreeTableView.setLoading(false);
            }
        };
        worker.execute();
    }

    public EntityTreeTableView getView(){
        return entityTreeTableView;
    }

    @Override
    @Inject
    public void setView(EntityTreeTableView entityTreeTableView) {
        this.entityTreeTableView = entityTreeTableView;
        addHandlers(entityTreeTableView);
    }

    private void addHandlers(EntityTreeTableView view){
        view.addRefreshButtonActionListener(event ->  refresh());
    }

}
