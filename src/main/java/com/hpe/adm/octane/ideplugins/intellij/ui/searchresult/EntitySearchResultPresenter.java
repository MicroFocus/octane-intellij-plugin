package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.google.common.eventbus.EventBus;
import com.google.inject.Inject;
import com.google.inject.name.Named;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.OpenDetailTabEvent;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.RefreshMyWorkEvent;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeView;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.MyWorkService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.nonentity.EntitySearchService;
import com.hpe.adm.octane.ideplugins.services.util.SdkUtil;
import com.intellij.icons.AllIcons;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.util.IconLoader;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import static com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil.getUiDataFromModel;

public class EntitySearchResultPresenter implements Presenter<EntityTreeView> {

    private static final EntityIconFactory entityIconFactory = new EntityIconFactory(20, 20, 11, Color.WHITE);

    protected EntityTreeView entityTreeView;

    @Inject
    private EntitySearchService entitySearchService;

    @Inject
    private MyWorkService myWorkService;

    @Inject
    private EventBus eventBus;

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
                //searchResults.addAll(entitySearchService.searchGlobal(query, Entity.DEFECT));
                searchResults.addAll(entitySearchService.searchGlobal(query, Entity.TASK));
                searchResults.addAll(entitySearchService.searchGlobal(query, Entity.TEST));
            }

            public void onSuccess() {
                entityTreeView.setLoading(false);
                entityTreeView.setTreeModel(createEmptyEntityTreeModel(searchResults));
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

        //eager init my work service support cache
        Arrays.asList(Entity.values()).forEach(myWorkService::isFollowingEntitySupported);
        setContextMenuFactory(this.entityTreeView);

        entityTreeView.setComponentWhenEmpty(() -> new NoSearchResultsPanel());
    }

    private EntityTreeModel createEmptyEntityTreeModel(Collection<EntityModel> entityModels){
        List<EntityTreeModel.EntityCategory> entityCategories = new ArrayList<>();
        entityCategories.add(new EntityTreeModel.EntityCategory("Backlog", Entity.USER_STORY, Entity.EPIC, Entity.FEATURE));
        entityCategories.add(new EntityTreeModel.EntityCategory("Defects", Entity.DEFECT));
        entityCategories.add(new EntityTreeModel.EntityCategory("Tasks", Entity.TASK));
        entityCategories.add(new EntityTreeModel.EntityCategory("Tests", Entity.GHERKIN_TEST, Entity.MANUAL_TEST));
        EntityTreeModel model = new EntityTreeModel(entityCategories, entityModels);
        return model;
    }

    @Inject
    private EntityService entityService;

    private void setContextMenuFactory(EntityTreeView entityTreeView) {
        entityTreeView.setEntityContextMenuFactory(entityModel -> {

            Entity entityType = Entity.getEntityType(entityModel);
            String entityName = UiUtil.getUiDataFromModel(entityModel.getValue("name"));
            Integer entityId = Integer.valueOf(getUiDataFromModel(entityModel.getValue("id")));

            JPopupMenu popup = new JPopupMenu();

            JMenuItem viewInBrowserItem = new JMenuItem("View in browser", IconLoader.findIcon(Constants.IMG_BROWSER_ICON));
            viewInBrowserItem.addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent mouseEvent) {
                    entityService.openInBrowser(entityModel);
                }
            });
            popup.add(viewInBrowserItem);

            if(entityType != Entity.COMMENT) {
                Icon icon = new ImageIcon(entityIconFactory.getIconAsImage(entityType));
                JMenuItem viewDetailMenuItem = new JMenuItem("View details", icon);
                viewDetailMenuItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        eventBus.post(new OpenDetailTabEvent(entityModel));
                    }
                });
                popup.add(viewDetailMenuItem);
            }

            if(myWorkService.isFollowingEntitySupported(entityType)) {
                JMenuItem addToMyWorkMenuItem = new JMenuItem("Add to \"My Work\"", AllIcons.General.Add);
                addToMyWorkMenuItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        ApplicationManager.getApplication().invokeLater(() -> {
                            Task.Backgroundable backgroundTask = new Task.Backgroundable(null, "Adding item to to \"My Work\"", true) {
                                public void run(@NotNull ProgressIndicator indicator) {
                                    if(myWorkService.addCurrentUserToFollowers(entityModel)) {
                                        eventBus.post(new RefreshMyWorkEvent());
                                        UiUtil.showWarningBalloon(null,
                                                "Item added",
                                                UiUtil.entityToString(entityModel),
                                                NotificationType.INFORMATION);
                                    } else {
                                        //also show a notification with the exception
                                        UiUtil.showWarningBalloon(null,
                                                "Item was not added, it is already in \"My Work\"",
                                                UiUtil.entityToString(entityModel),
                                                NotificationType.WARNING);
                                    }
                                }
                            };
                            backgroundTask.queue();
                        });
                    }
                });
                popup.add(addToMyWorkMenuItem);
            }

            return popup;
        });
    }
}
