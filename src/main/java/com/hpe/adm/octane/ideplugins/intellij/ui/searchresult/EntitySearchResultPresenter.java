/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors (“Open Text”) are as may be set forth
 * in the express warranty statements accompanying such products and services.
 * Nothing herein should be construed as constituting an additional warranty.
 * Open Text shall not be liable for technical or editorial errors or
 * omissions contained herein. The information contained herein is subject
 * to change without notice.
 *
 * Except as specifically indicated otherwise, this document contains
 * confidential information and a valid license is required for possession,
 * use or copying. If this work is provided to the U.S. Government,
 * consistent with FAR 12.211 and 12.212, Commercial Computer Software,
 * Computer Software Documentation, and Technical Data for Commercial Items are
 * licensed to the U.S. Government under vendor's standard commercial license.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

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
import com.hpe.adm.octane.ideplugins.intellij.ui.util.DownloadScriptUtil;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.services.EntityLabelService;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.mywork.MyWorkService;
import com.hpe.adm.octane.ideplugins.services.nonentity.EntitySearchService;
import com.hpe.adm.octane.ideplugins.services.nonentity.OctaneVersionService;
import com.hpe.adm.octane.ideplugins.services.util.OctaneVersion;
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
import java.util.*;

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

    @Inject
    private DownloadScriptUtil downloadScriptUtil;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    private String lastSearchQuery = null;

    @Override
    public EntityTreeView getView() {
        return entityTreeView;
    }

    public void globalSearch(String query) {
        String trimmedQuery = query.trim();
        lastSearchQuery = trimmedQuery;

        Task.Backgroundable backgroundTask = new Task.Backgroundable(null, "Searching Octane for \"" + trimmedQuery + "\"", false) {

            private Collection<EntityModel> searchResults;

            public void run(@NotNull ProgressIndicator indicator) {
                entityTreeView.setLoading(true);

                // add BDD to searchEntityTypes for Octane versions higher or eq than Coldplay P1 ( 15.1.4 - version where BDD was implemented )
                OctaneVersion octaneVersion = OctaneVersionService.getOctaneVersion(connectionSettingsProvider.getConnectionSettings());
                Entity[] searchEntityTypesCopy = searchEntityTypes;
                if (octaneVersion.isMoreOrEqThan(OctaneVersion.COLDPLAY_P1)) {
                    searchEntityTypesCopy = addEntityType(searchEntityTypesCopy, Entity.BDD_SCENARIO);
                }

                searchResults = entitySearchService.searchGlobal(trimmedQuery, 20, searchEntityTypesCopy);
            }

            public void onSuccess() {
                entityTreeView.setLoading(false);
                entityTreeView.setTreeModel(createEmptyEntityTreeModel(searchResults));
                entityTreeView.expandAllNodes();
            }

            public void onThrowable(@NotNull Throwable ex) {
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
        this.entityTreeView.addActionToToolbar(new AnAction("Refresh", "Refresh view", IconLoader.findIcon(Constants.IMG_REFRESH_ICON, EntitySearchResultPresenter.class.getClassLoader())) {
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

            JMenuItem viewInBrowserItem = new JMenuItem("View in browser", IconLoader.findIcon(Constants.IMG_BROWSER_ICON, EntitySearchResultPresenter.class.getClassLoader()));
            viewInBrowserItem.addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent mouseEvent) {
                    entityService.openInBrowser(entityModel);
                }
            });
            popup.add(viewInBrowserItem);

            if ( entityType == Entity.GHERKIN_TEST || entityType == Entity.BDD_SCENARIO ) {
                JMenuItem downloadScriptItem = new JMenuItem("Download script", AllIcons.Actions.Download);
                downloadScriptItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        super.mousePressed(e);
                        if (SwingUtilities.isLeftMouseButton(e))
                            downloadScriptUtil.downloadScriptForTest(entityModel);
                    }
                });
                popup.add(downloadScriptItem);
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

    private static Entity[] addEntityType(Entity searchEntityTypes[], Entity newType) {
        Entity searchEntityTypesCopy[] = new Entity[searchEntityTypes.length + 1];
        for (int i = 0; i < searchEntityTypes.length; i++) {
            if (searchEntityTypes[i] != newType) { // if the type was already added, return initial array
                searchEntityTypesCopy[i] = searchEntityTypes[i];
            }
            else {
                return searchEntityTypes;
            }
        }
        searchEntityTypesCopy[searchEntityTypes.length] = newType;

        return searchEntityTypesCopy;
    }
}
