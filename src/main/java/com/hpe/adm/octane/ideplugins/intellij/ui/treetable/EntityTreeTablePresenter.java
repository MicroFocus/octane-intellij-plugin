/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors ("Open Text") are as may be set forth
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

package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.common.eventbus.EventBus;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.google.inject.name.Named;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.OpenDetailTabEvent;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.RefreshMyWorkEvent;
import com.hpe.adm.octane.ideplugins.intellij.gitcommit.CommitMessageUtils;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.DownloadScriptUtil;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.services.CommentService;
import com.hpe.adm.octane.ideplugins.services.EntityLabelService;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.mywork.MyWorkService;
import com.hpe.adm.octane.ideplugins.services.mywork.MyWorkUtil;
import com.hpe.adm.octane.ideplugins.services.nonentity.OctaneVersionService;
import com.hpe.adm.octane.ideplugins.services.util.EntityUtil;
import com.hpe.adm.octane.ideplugins.services.util.OctaneVersion;
import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.icons.AllIcons;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.JBMenuItem;
import com.intellij.openapi.util.IconLoader;
import org.jetbrains.annotations.NotNull;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.*;
import java.util.stream.Collectors;

import static com.hpe.adm.octane.ideplugins.services.util.Util.getUiDataFromModel;

@Singleton
public class EntityTreeTablePresenter implements Presenter<EntityTreeView> {

    private EntityTreeView entityTreeTableView;

    @Inject
    private EntityIconFactory iconFactory;

    @Inject
    private MyWorkService myWorkService;

    @Inject
    private EntityService entityService;

    @Inject
    private EventBus eventBus;

    @Inject
    private CommitMessageUtils commitMessageUtils;

    @Inject
    private IdePluginPersistentState persistentState;

    @Inject
    private DownloadScriptUtil downloadScriptUtil;

    @Inject
    private Project project;

    @Inject
    private CommentService commentService;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    @Inject
    private OctaneVersionService versionService;

    public EntityTreeTablePresenter() {
    }

    public void refresh() {
        Task.Backgroundable backgroundTask = new Task.Backgroundable(project, "Loading \"My Work\"", false) {
            public void run(@NotNull ProgressIndicator indicator) {
                try {
                    entityTreeTableView.setLoading(true);

                    // remove BDD from EntityFieldMap for Octane versions lower than Coldplay P1 ( 15.1.4 - version where BDD was implemented )
                    OctaneVersion octaneVersion = OctaneVersionService.getOctaneVersion(connectionSettingsProvider.getConnectionSettings());
                    if (octaneVersion.isLessThan(OctaneVersion.COLDPLAY_P1)) {
                        EntityTreeCellRenderer.removeBddFromEntityFields();
                    }

                    Collection<EntityModel> myWork = myWorkService.getMyWork(EntityTreeCellRenderer.getEntityFieldMap());

                    SwingUtilities.invokeLater(() -> {
                        entityTreeTableView.setLoading(false);
                        entityTreeTableView.setTreeModel(createEntityTreeModel(myWork));
                        entityTreeTableView.expandAllNodes();
                        updateActiveItem(myWork);
                    });

                } catch (Exception ex) {
                    entityTreeTableView.setLoading(false);
                    String message = ex.getMessage();
                    if (ex instanceof OctaneException) {
                        OctaneException octaneException = (OctaneException) ex;
                        StringFieldModel errorDescription = (StringFieldModel) octaneException.getError().getValue("description");
                        if (errorDescription != null) {
                            message = errorDescription.getValue();
                        }
                    }
                    entityTreeTableView.setErrorMessage("Failed to load \"My work\" <br>" + message, project);
                }
            }
        };
        backgroundTask.queue();
    }

    /**
     * Careful, this method works with the work items directly, not with user items
     * Clear active item if it's not in my work
     */
    private void updateActiveItem(Collection<EntityModel> myWork) {

        //Convert to normal entities
        if (!isIronMaidenP1OrHigher()) {
            myWork = MyWorkUtil.getEntityModelsFromUserItems(myWork);
        }

        PartialEntity activeItem = getActiveItemFromPersistentState();
        if (activeItem == null) return;

        boolean clearActiveItem;

        if (myWork != null) {
            Optional<EntityModel> activeItemInMyWork = myWork
                    .stream()
                    .filter(entityModel -> EntityUtil.areEqual(entityModel, activeItem))
                    .findFirst();

            activeItemInMyWork.ifPresent(entityModel -> {
                //Refresh the name of the entity model, in case it has changed. The name is stored in the IntelliJ cache
                activeItem.setEntityName(entityModel.getValue("name").getValue().toString());
                persistentState.saveState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM, PartialEntity.toJsonObject(activeItem));
            });

            clearActiveItem = !activeItemInMyWork.isPresent();
        } else {
            clearActiveItem = true;
        }

        if (clearActiveItem) {
            persistentState.clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
            UiUtil.showWarningBalloon(null,
                    "Active item cleared, no longer part of \"My Work\"",
                    "Active item: \""
                            + activeItem.getEntityType().getEntityName()
                            + " " + activeItem.getEntityId() + ": "
                            + " " + activeItem.getEntityName()
                            + "\" has been removed, it is no longer part of \"My Work\"",
                    NotificationType.INFORMATION);
        }
    }

    public EntityTreeView getView() {
        return entityTreeTableView;
    }

    @Override
    @Inject
    public void setView(@Named("myWorkEntityTreeView") EntityTreeView entityTreeView) {
        this.entityTreeTableView = entityTreeView;

        //eager init my work service support cache
        //Arrays.asList(Entity.values()).forEach(myWorkService::isFollowingEntitySupported);
        setContextMenuFactory(entityTreeView);

        //start presenting
        entityTreeTableView.addActionToToolbar(new AnAction("Refresh", "Refresh view", IconLoader.findIcon(Constants.IMG_REFRESH_ICON, EntityTreeTablePresenter.class.getClassLoader())) {
            @Override
            public void actionPerformed(AnActionEvent e) {
                refresh();
            }
        });

        entityTreeTableView.addSeparatorToToolbar();
        entityTreeTableView.addActionToToolbar(new EntityTreeView.ExpandNodesAction(entityTreeTableView));
        entityTreeTableView.addActionToToolbar(new EntityTreeView.CollapseNodesAction(entityTreeTableView));
        entityTreeTableView.addSeparatorToToolbar();

        //Also register event handler
        eventBus.register((RefreshMyWorkEvent.RefreshMyWorkEventListener) refreshMyWorkEvent -> refresh());

        refresh();
    }

    private void setContextMenuFactory(EntityTreeView entityTreeView) {
        entityTreeView.setEntityContextMenuFactory(userItem -> {
            EntityModel entityModel;

            if (!isIronMaidenP1OrHigher() && Entity.USER_ITEM == Entity.getEntityType(userItem)) {
                entityModel = MyWorkUtil.getEntityModelFromUserItem(userItem);
            } else {
                entityModel = userItem;
            }

            Entity entityType = Entity.getEntityType(entityModel);
            String entityName = Util.getUiDataFromModel(entityModel.getValue("name"));
            Integer entityId = Integer.valueOf(getUiDataFromModel(entityModel.getValue("id")));

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

            if (entityType == Entity.TASK || entityType == Entity.COMMENT) {
                //Get parent info
                EntityModel parentEntityModel;
                if (entityType == Entity.TASK) {
                    parentEntityModel = (EntityModel) entityModel.getValue("story").getValue();
                } else {
                    parentEntityModel = (EntityModel) Util.getContainerItemForCommentModel(entityModel).getValue();
                }

                if (TabbedPanePresenter.isDetailTabSupported(Entity.getEntityType(parentEntityModel))) {
                    //Add option
                    Icon icon = new ImageIcon(iconFactory.getIconAsImage(Entity.getEntityType(parentEntityModel), 20, 11));
                    JMenuItem viewParentMenuItem = new JMenuItem("View parent details", icon);
                    viewParentMenuItem.addMouseListener(new MouseAdapter() {
                        @Override
                        public void mousePressed(MouseEvent e) {
                            eventBus.post(new OpenDetailTabEvent(parentEntityModel));
                        }
                    });
                    popup.add(viewParentMenuItem);
                }
            }

            JMenuItem viewInBrowserItem = new JMenuItem("View in browser", IconLoader.findIcon(Constants.IMG_BROWSER_ICON, EntityTreeTablePresenter.class.getClassLoader()));
            viewInBrowserItem.addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent mouseEvent) {
                    entityService.openInBrowser(entityModel);
                }
            });
            popup.add(viewInBrowserItem);

            if (entityType == Entity.GHERKIN_TEST || entityType == Entity.BDD_SCENARIO) {
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

            if (entityType == Entity.DEFECT ||
                    entityType == Entity.USER_STORY ||
                    entityType == Entity.QUALITY_STORY ||
                    entityType == Entity.TASK) {

                popup.addSeparator();

                PartialEntity selectedItem = new PartialEntity(entityId.longValue(), entityName, entityType);
                PartialEntity currentActiveItem = getActiveItemFromPersistentState();

                boolean isActivated = selectedItem.equals(currentActiveItem);

                JMenuItem activateItem;
                if (isActivated) {
                    activateItem = new JMenuItem("Stop work", IconLoader.findIcon(Constants.IMG_STOP_TIMER, EntityTreeTablePresenter.class.getClassLoader()));

                } else {
                    activateItem = new JMenuItem("Start work", IconLoader.findIcon(Constants.IMG_START_TIMER, EntityTreeTablePresenter.class.getClassLoader()));
                }

                activateItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        super.mousePressed(e);
                        if (isActivated) {
                            persistentState.clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
                            persistentState.clearState(IdePluginPersistentState.Key.PREV_ACTIVE_WORK_ITEM);
                        } else {
                            setActiveItemFromPersistentState(selectedItem);
                        }
                    }
                });
                popup.add(activateItem);


                JMenuItem copyCommitMessage = new JBMenuItem("Copy Commit Message", IconLoader.findIcon(Constants.IMG_COPY_ICON, EntityTreeTablePresenter.class.getClassLoader()));
                copyCommitMessage.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        super.mousePressed(e);
                        commitMessageUtils.asyncCopyCommitMessageToClipboard(selectedItem);
                    }
                });
                popup.add(copyCommitMessage);

            }

            if (entityType == Entity.COMMENT) {
                JMenuItem removeFromMyWorkMenuItem = new JMenuItem("Dismiss", AllIcons.General.Remove);
                removeFromMyWorkMenuItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        ApplicationManager.getApplication().invokeLater(() -> {
                            Task.Backgroundable backgroundTask =

                                    new Task.Backgroundable(
                                            null,
                                            "Dismissing item from \"My Work\"",
                                            true) {

                                        public void run(@NotNull ProgressIndicator indicator) {
                                            if (commentService.dismissComment(entityModel)) {

                                                String userItemId;

                                                if (isIronMaidenP1OrHigher()) {
                                                    userItemId = userItem.getId();
                                                } else {
                                                    userItemId = (String) ((ReferenceFieldModel) userItem.getValue("my_follow_items_comment")).getValue().getValue("id").getValue();
                                                }

                                                //Remove dismissed item if successful
                                                List<EntityModel> list = entityTreeView.getTreeModel().getGroupedEntities()
                                                        .values()
                                                        .stream()
                                                        .flatMap(Collection::stream)
                                                        .filter(currentEntityModel -> {
                                                            if (isIronMaidenP1OrHigher()) {
                                                                boolean isComment = currentEntityModel.getType().equals("comment");

                                                                return !isComment || !userItemId.equals(currentEntityModel.getId());
                                                            } else {
                                                                boolean isComment = (MyWorkUtil.getEntityModelFromUserItem(currentEntityModel)).getType().equals("comment");
                                                                String myFollowEntityModelId = (MyWorkUtil.getEntityModelFromUserItem(currentEntityModel)).getId();

                                                                return !isComment || !userItemId.equals(myFollowEntityModelId);
                                                            }
                                                        })
                                                        .collect(Collectors.toList());

                                                SwingUtilities.invokeLater(() -> {
                                                    updateActiveItem(list);
                                                    entityTreeView.setTreeModel(createEntityTreeModel(list));
                                                    entityTreeView.expandAllNodes();
                                                });

                                                //refresh();
                                                UiUtil.showWarningBalloon(null,
                                                        "Comment dismissed",
                                                        UiUtil.entityToString(entityModel) + entityModel.getValue("text").getValue(),
                                                        NotificationType.INFORMATION);
                                            }
                                        }

                                    };

                            backgroundTask.queue();
                        });
                    }
                });
                popup.add(removeFromMyWorkMenuItem);
            }

            if (myWorkService.isAddingToMyWorkSupported(entityType)) {

                JMenuItem removeFromMyWorkMenuItem = new JMenuItem("Dismiss", AllIcons.General.Remove);
                removeFromMyWorkMenuItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        ApplicationManager.getApplication().invokeLater(() -> {
                            Task.Backgroundable backgroundTask =

                                    new Task.Backgroundable(
                                            null,
                                            "Dismissing item from \"My Work\"",
                                            true) {

                                        public void run(@NotNull ProgressIndicator indicator) {
                                            if (myWorkService.removeFromMyWork(entityModel)) {

                                                //Remove dismissed item if successful
                                                List<EntityModel> list = entityTreeView.getTreeModel().getGroupedEntities()
                                                        .values()
                                                        .stream()
                                                        .flatMap(Collection::stream)
                                                        .filter(currentEntityModel ->
                                                                !EntityUtil.areEqual(currentEntityModel, userItem))
                                                        .collect(Collectors.toList());

                                                SwingUtilities.invokeLater(() -> {
                                                    updateActiveItem(list);
                                                    entityTreeView.setTreeModel(createEntityTreeModel(list));
                                                    entityTreeView.expandAllNodes();
                                                });

                                                //refresh();
                                                UiUtil.showWarningBalloon(null,
                                                        "Item dismissed",
                                                        UiUtil.entityToString(entityModel),
                                                        NotificationType.INFORMATION);
                                            }
                                        }

                                    };

                            backgroundTask.queue();
                        });
                    }
                });
                popup.add(removeFromMyWorkMenuItem);
            }

            return popup;
        });
    }

    private PartialEntity getActiveItemFromPersistentState() {
        JSONObject jsonObject = persistentState.loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
        if (jsonObject == null) {
            return null;
        } else {
            return PartialEntity.fromJsonObject(jsonObject);
        }
    }

    private void setActiveItemFromPersistentState(PartialEntity entityTypeIdPair) {
        persistentState.saveState(
                IdePluginPersistentState.Key.ACTIVE_WORK_ITEM,
                PartialEntity.toJsonObject(entityTypeIdPair));
    }


    public void addEntityClickHandler(EntityTreeView.EntityDoubleClickHandler handler) {
        getView().addEntityMouseHandler(handler);
    }

    public void addEntityKeyHandler(EntityTreeView.TreeViewKeyHandler handler) {
        getView().addEntityKeyHandler(handler);
    }

    private EntityTreeModel createEntityTreeModel(Collection<EntityModel> entityModels) {
        List<EntityCategory> entityCategories = new ArrayList<>();

        EntityLabelService entityLabelService = PluginModule.getPluginModuleForProject(project).getInstance(EntityLabelService.class);
        Map<Entity, EntityModel> entityLabelMap = entityLabelService.getEntityLabelDetails();

        entityCategories.add(new UserItemEntityCategory("Backlog",
                Entity.USER_STORY,
                Entity.DEFECT,
                Entity.QUALITY_STORY));

        entityCategories.add(new UserItemEntityCategory(entityLabelMap.get(Entity.REQUIREMENT).getValue("plural_capitalized").getValue().toString(), Entity.REQUIREMENT));
        entityCategories.add(new UserItemEntityCategory(entityLabelMap.get(Entity.TASK).getValue("plural_capitalized").getValue().toString(), Entity.TASK));
        entityCategories.add(new UserItemEntityCategory("Test runs", Entity.MANUAL_TEST_RUN, Entity.TEST_SUITE_RUN));
        entityCategories.add(new UserItemEntityCategory("Tests", Entity.GHERKIN_TEST, Entity.MANUAL_TEST, Entity.BDD_SCENARIO));
        entityCategories.add(new UserItemEntityCategory("Mention in comments", Entity.COMMENT));

        return new EntityTreeModel(entityCategories, entityModels);
    }

    private boolean isIronMaidenP1OrHigher() {
        return OctaneVersion.compare(versionService.getOctaneVersion(), OctaneVersion.Operation.HIGHER_EQ, OctaneVersion.IRONMAIDEN_P1);
    }
}
