package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.filtering.Filter;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import java.util.*;

public class EntityTreeTablePresenter implements Presenter<EntityTreeView>{

    EntityTreeView entityTreeTableView;
    EntityTreeModel entityTreeModel = new EntityTreeModel();

    @Inject
    EntityService entityService;

    public EntityTreeTablePresenter(){
    }

    public void refresh(){

        //refine these with a more complex filter (for selecting my work)
        Map<Entity, List<Filter>> map = new HashMap<>();
        map.put(Entity.DEFECT, new ArrayList<>());
        map.put(Entity.STORY, new ArrayList<>());
        map.put(Entity.TEST, new ArrayList<>());
        map.put(Entity.TASK, new ArrayList<>());


        DefaultMutableTreeNode root = new DefaultMutableTreeNode();

        DefaultMutableTreeNode story = new DefaultMutableTreeNode();

        Collection<EntityModel> entities = entityService.findEntities(map);

        for(EntityModel em : entities){
            String name = em.getValue("name").getValue().toString();
            story.add(new DefaultMutableTreeNode(name));
        }

        root.add(story);

        DefaultTreeModel model = new DefaultTreeModel(root);

        entityTreeTableView.setTreeModel(model);

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
