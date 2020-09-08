/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.google.common.eventbus.EventBus;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.google.inject.name.Named;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.OpenDetailTabEvent;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.RefreshMyWorkEvent;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityCategory;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeView;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.intellij.util.ExceptionHandler;
import com.hpe.adm.octane.ideplugins.services.EntityLabelService;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.mywork.MyWorkService;
import com.hpe.adm.octane.ideplugins.services.nonentity.EntitySearchService;
import com.intellij.icons.AllIcons;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

@Singleton
public class EntitySearchResultPresenter implements Presenter<EntityTreeView> {

    @Inject
    private EntityIconFactory iconFactory;

    @Inject
    private EntityLabelService entityLabelService;

    private static final Entity[] searchEntityTypes = new Entity[]{
            Entity.EPIC,
            Entity.FEATURE,
            Entity.USER_STORY,
            Entity.DEFECT,
            Entity.QUALITY_STORY,
            Entity.TASK,
            Entity.TEST_SUITE,
            Entity.MANUAL_TEST,
            Entity.AUTOMATED_TEST,
            Entity.GHERKIN_TEST,
            Entity.BDD_SCENARIO,
            Entity.REQUIREMENT};

    protected EntityTreeView entityTreeView;

    @Inject
    private EntitySearchService entitySearchService;

    @Inject
    private MyWorkService myWorkService;

    @Inject
    private Project project;

    @Inject
    private EventBus eventBus;

    private String lastSearchQuery = null;

    @Override
    public EntityTreeView getView() {
        return entityTreeView;
    }

    public void globalSearch(String query) {

        lastSearchQuery = query;

        Task.Backgroundable backgroundTask = new Task.Backgroundable(null, "Searching Octane for \"" + query + "\"", false) {

            private Collection<EntityModel> searchResults;

            public void run(@NotNull ProgressIndicator indicator) {
                entityTreeView.setLoading(true);
                searchResults = entitySearchService.searchGlobal(query, 20, searchEntityTypes);
            }

            public void onSuccess() {
                entityTreeView.setLoading(false);
                entityTreeView.setTreeModel(createEmptyEntityTreeModel(searchResults));
                entityTreeView.expandAllNodes();
            }

            public void onError(@NotNull Exception ex) {
                entityTreeView.setLoading(false);
                String message = ex.getMessage();
                if (ex instanceof OctaneException) {
                    entityTreeView.setTreeModel(createEmptyEntityTreeModel(new ArrayList<>()));
                    OctaneException octaneException = (OctaneException) ex;
                    StringFieldModel errorDescription = (StringFieldModel) octaneException.getError().getValue("description");
                    if (errorDescription != null) {
                        message = errorDescription.getValue();
                    }
                }
                entityTreeView.setErrorMessage(message, project);

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
        //Arrays.asList(Entity.values()).forEach(myWorkService::isFollowingEntitySupported);
        setContextMenuFactory(this.entityTreeView);

        entityTreeView.setComponentWhenEmpty(NoSearchResultsPanel::new);
    }

    private EntityTreeModel createEmptyEntityTreeModel(Collection<EntityModel> entityModels) {
        List<EntityCategory> entityCategories = new ArrayList<>();
        Map<Entity, EntityModel> entityLabelMap = entityLabelService.getEntityLabelDetails();
        entityCategories.add(new SearchEntityCategory("Backlog", Entity.USER_STORY, Entity.EPIC, Entity.FEATURE, Entity.QUALITY_STORY));
        entityCategories.add(new SearchEntityCategory(entityLabelMap.get(Entity.REQUIREMENT).getValue("plural_capitalized").getValue().toString(), Entity.REQUIREMENT));
        entityCategories.add(new SearchEntityCategory(entityLabelMap.get(Entity.DEFECT).getValue("plural_capitalized").getValue().toString(), Entity.DEFECT));
        entityCategories.add(new SearchEntityCategory(entityLabelMap.get(Entity.TASK).getValue("plural_capitalized").getValue().toString(), Entity.TASK));
        entityCategories.add(new SearchEntityCategory("Tests", Entity.TEST_SUITE, Entity.MANUAL_TEST, Entity.AUTOMATED_TEST, Entity.GHERKIN_TEST, Entity.BDD_SCENARIO));
        return new EntityTreeModel(entityCategories, entityModels);
    }

    @Inject
    private EntityService entityService;

    private void setContextMenuFactory(EntityTreeView entityTreeView) {
        entityTreeView.setEntityContextMenuFactory(entityModel -> {

            Entity entityType = Entity.getEntityType(entityModel);
            JPopupMenu popup = new JPopupMenu();

            JMenuItem viewInBrowserItem = new JMenuItem("View in browser", IconLoader.findIcon(Constants.IMG_BROWSER_ICON));
            viewInBrowserItem.addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent mouseEvent) {
                    entityService.openInBrowser(entityModel);
                }
            });
            popup.add(viewInBrowserItem);

            if (TabbedPanePresenter.isDetailTabSupported(entityType)) {
                Icon icon = new ImageIcon(iconFactory.getIconAsImage(entityType, 20, 11));
                JMenuItem viewDetailMenuItem = new JMenuItem("View details", icon);
                viewDetailMenuItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        eventBus.post(new OpenDetailTabEvent(entityModel));
                    }
                });
                popup.add(viewDetailMenuItem);
            }

            if (myWorkService.isAddingToMyWorkSupported(entityType)) {
                JMenuItem addToMyWorkMenuItem = new JMenuItem("Add to \"My Work\"", AllIcons.General.Add);
                addToMyWorkMenuItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        ApplicationManager.getApplication().invokeLater(() -> {
                            Task.Backgroundable backgroundTask = new Task.Backgroundable(null, "Adding item to \"My Work\"", true) {
                                public void run(@NotNull ProgressIndicator indicator) {
                                    if (myWorkService.addToMyWork(entityModel)) {
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
