package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

import javax.swing.*;

/**
 * Created by tothan on 11/22/2016.
 */
public class EntityTreeTablePresenter {

    EntityTreeTableModel treeTableModel;
    EntityTreeTableView entityTreeTableView;

    TestService testService = PluginModule.getInstance(TestService.class);

    public EntityTreeTablePresenter(){
        treeTableModel = new EntityTreeTableModel();
        entityTreeTableView = new EntityTreeTableView(this, treeTableModel);
    }

    public void refresh(){
        //Async get
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
            @Override
            protected Void doInBackground() throws Exception {
                entityTreeTableView.setLoading(true);
                treeTableModel.setEntities(testService.findEntities(Entity.WORK_ITEM), "subtype");
                return null;
            }
            @Override
            protected void done(){
                entityTreeTableView.setLoading(false);
            }
        };
        worker.execute();
    }

    public HasComponent getView(){
        return entityTreeTableView;
    }

}
