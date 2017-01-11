package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeView;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.nonentity.EntitySearchService;
import com.hpe.adm.octane.ideplugins.services.util.SdkUtil;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.util.IconLoader;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

public class EntitySearchResultPresenter implements Presenter<EntityTreeView> {

    protected EntityTreeView entityTreeView;

    @Inject
    private EntitySearchService entitySearchService;

    private String lastSearchQuery = null;

    @Override
    public EntityTreeView getView() {
        return entityTreeView;
    }

    public void globalSearch(String query){

        lastSearchQuery = query;

        Task.Backgroundable backgroundTask = new Task.Backgroundable(null, "Searching Octane for \""+query+"\"", false) {

            private Collection<EntityModel> searchResults;

            public void run(@NotNull ProgressIndicator indicator) {
                entityTreeView.setLoading(true);

                //TODO: add supported entities when stories are completed
                searchResults = entitySearchService.searchGlobal(query, Entity.WORK_ITEM);
                searchResults.addAll(entitySearchService.searchGlobal(query, Entity.DEFECT));
                searchResults.addAll(entitySearchService.searchGlobal(query, Entity.TASK));
            }

            public void onSuccess() {
                entityTreeView.setLoading(false);
                entityTreeView.setTreeModel(new EntityTreeModel(searchResults));
                entityTreeView.expandAllNodes();
            }

            public void onError(@NotNull Exception ex) {
                entityTreeView.setLoading(false);

                String message;
                if(ex instanceof OctaneException){
                    message = SdkUtil.getMessageFromOctaneException((OctaneException) ex);
                } else {
                    message = ex.getMessage();
                }
                entityTreeView.setErrorMessage("Search failed <br>" + message);
            }
        };

        backgroundTask.queue();
    }

    @Override
    @Inject
    public void setView(@Named("searchEntityTreeView") EntityTreeView entityTreeView) {
        this.entityTreeView = entityTreeView;

        //start presenting
        this.entityTreeView.addActionToToolbar(new AnAction("Refresh", "Refresh view", IconLoader.findIcon(Constants.IMG_REFRESH_ICON)) {
            @Override
            public void actionPerformed(AnActionEvent e) {
                globalSearch(lastSearchQuery);
            }
        });

        this.entityTreeView.addSeparatorToToolbar();
        this.entityTreeView.addActionToToolbar(new EntityTreeView.ExpandNodesAction(this.entityTreeView));
        this.entityTreeView.addActionToToolbar(new EntityTreeView.CollapseNodesAction(this.entityTreeView));
        this.entityTreeView.addSeparatorToToolbar();

        entityTreeView.setComponentWhenEmpty(new NoSearchResultsPanel());
    }
}
