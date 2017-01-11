package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.ToolbarActiveItem;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.util.SdkUtil;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.util.IconLoader;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

public class EntityTreeTablePresenter implements Presenter<EntityTreeView>{

    EntityTreeView entityTreeTableView;

    @Inject
    EntityService entityService;

    public EntityTreeTablePresenter(){
    }

    public void refresh(){

        // Collection<EntityModel> myWork = entityService.getMyWork();
        // entityTreeModel.setEntities(myWork);
        // entityTreeTableView.setTreeModel(entityTreeModel);

        Task.Backgroundable backgroundTask = new Task.Backgroundable(null, "Loading \"My work\"", false) {

            private Collection<EntityModel> myWork;

            public void run(@NotNull ProgressIndicator indicator) {
                entityTreeTableView.setLoading(true);
                myWork = entityService.getMyWork(EntityTreeCellRenderer.getEntityFieldMap());

            }

            public void onSuccess() {
                entityTreeTableView.setLoading(false);

                entityTreeTableView.setTreeModel(new EntityTreeModel(myWork));
                entityTreeTableView.expandAllNodes();
                ToolbarActiveItem.getInstance().update(myWork);
            }

            public void onError(@NotNull Exception ex) {
                entityTreeTableView.setLoading(false);

                String message;
                if(ex instanceof OctaneException){
                    message = SdkUtil.getMessageFromOctaneException((OctaneException) ex);
                } else {
                    message = ex.getMessage();
                }
                entityTreeTableView.setErrorMessage("Failed to load \"My work\" <br>" + message);
            }
        };

        backgroundTask.queue();
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

    public void addEntityClickHandler(EntityTreeView.EntityDoubleClickHandler handler) {
        getView().addEntityMouseHandler(handler);
    }

    public void addEntityKeyHandler(EntityTreeView.TreeViewKeyHandler handler) {
        getView().addEntityKeyHandler(handler);
    }
}
