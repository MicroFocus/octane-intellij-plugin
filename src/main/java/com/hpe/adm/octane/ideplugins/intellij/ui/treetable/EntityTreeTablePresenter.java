package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

import javax.swing.*;
import java.util.Collection;

public class EntityTreeTablePresenter implements Presenter<EntityTreeView>{

    EntityTreeView entityTreeTableView;

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
                    SwingUtilities.invokeLater(() -> entityTreeTableView.setTreeModel(new EntityTreeModel(myWork)));
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

    private final class RefreshAction extends AnAction {
        public RefreshAction() {
            super("Refresh", "Refresh view", IconLoader.findIcon(Constants.IMG_REFRESH_ICON));
        }

        public void actionPerformed(AnActionEvent e) {
            refresh();
        }
    }

    private final class ExpandNodesAction extends AnAction {
        public ExpandNodesAction() {
            super("Expand all", "Expand all nodes of the tree", AllIcons.Actions.Expandall);
        }

        public void actionPerformed(AnActionEvent e) {
            getView().expandAllNodes();
        }

    }

    private final class CollapseNodesAction extends AnAction {
        public CollapseNodesAction() {
            super("Collapse all", "Collapse all nodes of the tree", AllIcons.Actions.Collapseall);
        }
        public void actionPerformed(AnActionEvent e) {
            getView().collapseAllNodes();
        }
    }


    public EntityTreeView getView(){
        return entityTreeTableView;
    }

    @Override
    @Inject
    public void setView(EntityTreeView entityTreeView) {
        this.entityTreeTableView = entityTreeView;

        //start presenting
        entityTreeTableView.addActionToToolbar(new RefreshAction());
        entityTreeTableView.addSeparatorToToolbar();
        entityTreeTableView.addActionToToolbar(new ExpandNodesAction());
        entityTreeTableView.addActionToToolbar(new CollapseNodesAction());
        entityTreeTableView.addSeparatorToToolbar();
        refresh();
    }

    public void addEntityDoubleClickHandler(EntityTreeView.EntityDoubleClickHandler handler) {
        getView().addEntityDoubleClickHandler(handler);
    }
}
