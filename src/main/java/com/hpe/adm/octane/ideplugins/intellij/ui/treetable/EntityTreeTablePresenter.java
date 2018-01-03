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

package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.common.eventbus.EventBus;
import com.google.inject.Inject;
import com.google.inject.name.Named;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.OpenDetailTabEvent;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.RefreshMyWorkEvent;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.intellij.util.RestUtil;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.mywork.MyWorkService;
import com.hpe.adm.octane.ideplugins.services.mywork.MyWorkUtil;
import com.hpe.adm.octane.ideplugins.services.nonentity.DownloadScriptService;
import com.hpe.adm.octane.ideplugins.services.util.EntityUtil;
import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
import com.hpe.adm.octane.ideplugins.services.util.SdkUtil;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.icons.AllIcons;
import com.intellij.ide.actions.OpenProjectFileChooserDescriptor;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileChooser.FileChooser;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.vcs.VcsShowConfirmationOption;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ui.ConfirmationDialog;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static com.hpe.adm.octane.ideplugins.services.util.Util.getUiDataFromModel;

public class EntityTreeTablePresenter implements Presenter<EntityTreeView> {

    private static final EntityIconFactory entityIconFactory = new EntityIconFactory(20, 20, 11, Color.WHITE);

    private EntityTreeView entityTreeTableView;

    @Inject
    private MyWorkService myWorkService;

    @Inject
    private EntityService entityService;

    @Inject
    private EventBus eventBus;

    @Inject
    private IdePluginPersistentState persistentState;

    @Inject
    private DownloadScriptService scriptService;

    @Inject
    private Project project;

    public EntityTreeTablePresenter(){
    }

    public void refresh(){
        Task.Backgroundable backgroundTask = new Task.Backgroundable(project, "Loading \"My Work\"", false) {
            public void run(@NotNull ProgressIndicator indicator) {
                try {
                    entityTreeTableView.setLoading(true);
                    Collection<EntityModel> myWork = myWorkService.getMyWork(EntityTreeCellRenderer.getEntityFieldMap());
                    SwingUtilities.invokeLater(() -> {
                        entityTreeTableView.setLoading(false);
                        entityTreeTableView.setTreeModel(createEntityTreeModel(myWork));
                        entityTreeTableView.expandAllNodes();
                        updateActiveItem(myWork);
                    });

                } catch (Exception ex) {
                    entityTreeTableView.setLoading(false);
                    String message;
                    if (ex instanceof OctaneException) {
                        message = SdkUtil.getMessageFromOctaneException((OctaneException) ex);
                    } else {
                        message = ex.getMessage();
                    }
                    entityTreeTableView.setErrorMessage("Failed to load \"My work\" <br>" + message);
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
        myWork = MyWorkUtil.getEntityModelsFromUserItems(myWork);

        PartialEntity activeItem = getActiveItemFromPersistentState();
        if(activeItem == null) return;

        boolean clearActiveItem;

        if (myWork != null) {
            Optional<EntityModel> activeItemInMyWork = myWork
                    .stream()
                    .filter(entityModel ->  EntityUtil.areEqual(entityModel, activeItem))
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

        if(clearActiveItem){
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
        entityTreeTableView.addActionToToolbar(new AnAction("Refresh", "Refresh view", IconLoader.findIcon(Constants.IMG_REFRESH_ICON)) {
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

            EntityModel entityModel = MyWorkUtil.getEntityModelFromUserItem(userItem);
            Entity entityType = Entity.getEntityType(entityModel);
            String entityName = Util.getUiDataFromModel(entityModel.getValue("name"));
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

            if (entityType == Entity.TASK || entityType == Entity.COMMENT) {
                //Get parent info
                EntityModel parentEntityModel;
                if(entityType == Entity.TASK){
                    parentEntityModel = (EntityModel) entityModel.getValue("story").getValue();
                } else {
                    parentEntityModel = (EntityModel) Util.getContainerItemForCommentModel(entityModel).getValue();
                }

                //Add option
                Icon icon = new ImageIcon(entityIconFactory.getIconAsImage(Entity.getEntityType(parentEntityModel)));
                JMenuItem viewParentMenuItem = new JMenuItem("View parent details", icon);
                viewParentMenuItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        eventBus.post(new OpenDetailTabEvent(parentEntityModel));
                    }
                });
                popup.add(viewParentMenuItem);
            }

            if (entityType == Entity.GHERKIN_TEST) {
                JMenuItem downloadScriptItem = new JMenuItem("Download script", AllIcons.Actions.Download);
                downloadScriptItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        super.mousePressed(e);
                        if (SwingUtilities.isLeftMouseButton(e))
                            downloadScriptForGherkinTest(entityModel);
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
                    activateItem = new JMenuItem("Stop work", IconLoader.findIcon(Constants.IMG_STOP_TIMER));
                } else {
                    activateItem = new JMenuItem("Start work", IconLoader.findIcon(Constants.IMG_START_TIMER));
                }

                activateItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        super.mousePressed(e);
                        if (isActivated) {
                            persistentState.clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
                        } else {
                            setActiveItemFromPersistentState(selectedItem);
                        }
                    }
                });
                popup.add(activateItem);
            }

            if(myWorkService.isAddingToMyWorkSupported(entityType) && MyWorkUtil.isUserItemDismissible(userItem)) {

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
                                    if(myWorkService.removeFromMyWork(entityModel)) {

                                        //Remove dismissed item if successful
                                        List list = entityTreeView.getTreeModel().getGroupedEntities()
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

    private void downloadScriptForGherkinTest(EntityModel gherkinTest) {
        VirtualFile selectedFolder = chooseScriptFolder(project);
        if (selectedFolder != null) {
            long gherkinTestId = Long.parseLong(gherkinTest.getValue("id").getValue().toString());
            String gherkinTestName = gherkinTest.getValue("name").getValue().toString();
            String scriptFileName = gherkinTestName + "-" + gherkinTestId + ".feature";
            boolean shouldDownloadScript = true;
            if (selectedFolder.findChild(scriptFileName) != null) {
                String title = "Confirm file overwrite";
                String message = "Selected destination folder already contains a file named \"" +
                        scriptFileName + "\". Do you want to overwrite this file?";

                ConfirmationDialog dialog = new ConfirmationDialog(project, message, title,
                        null, VcsShowConfirmationOption.STATIC_SHOW_CONFIRMATION) {
                    @Override
                    public void setDoNotAskOption(@Nullable DoNotAskOption doNotAsk) {
                        super.setDoNotAskOption(null);
                    }
                };
                shouldDownloadScript = dialog.showAndGet();
            }

            if (shouldDownloadScript) {
                RestUtil.LOADING_MESSAGE = "Downloading script for gherkin test with id " + gherkinTestId;
                RestUtil.runInBackground(
                        () -> {
                            String scriptContent = scriptService.getGherkinTestScriptContent(gherkinTestId);
                            return createTestScriptFile(selectedFolder.getPath(), scriptFileName, scriptContent);
                        },
                        (scriptFile) -> {
                            VirtualFile vFile = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(scriptFile);
                            FileEditorManager.getInstance(project).openFile(vFile, true, true);
                            project.getBaseDir().refresh(false, true);
                        },
                        project,
                        "failed to download script for gherkin test with id " + gherkinTestId);
            }
        }
    }

    private VirtualFile chooseScriptFolder(Project project) {
        FileChooserDescriptor descriptor = new OpenProjectFileChooserDescriptor(true);
        descriptor.setHideIgnored(false);
        descriptor.setRoots(project.getBaseDir());
        descriptor.setTitle("Select Parent Folder");
        descriptor.withTreeRootVisible(false);
        VirtualFile[] virtualFile = FileChooser.chooseFiles(descriptor, null, null);

        return (virtualFile.length != 1) ? null : virtualFile[0];
    }

    private File createTestScriptFile(String path, String fileName, String script) {
        File f = new File(path + "/" + fileName.replaceAll("[\\\\/:?*\"<>|]", ""));
        try {
            f.createNewFile();
            if (script != null) {
                Writer out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f), StandardCharsets.UTF_8));
                out.append(script);
                out.flush();
                out.close();
            }
        } catch (IOException e) {
            return null;
        }
        return f;
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

    private static EntityTreeModel createEntityTreeModel(Collection<EntityModel> entityModels){
        List<EntityCategory> entityCategories = new ArrayList<>();
        entityCategories.add(new UserItemEntityCategory("Backlog", Entity.USER_STORY, Entity.DEFECT, Entity.QUALITY_STORY,
                Entity.EPIC, Entity.FEATURE));
        entityCategories.add(new UserItemEntityCategory("Requirements", Entity.REQUIREMENT));
        entityCategories.add(new UserItemEntityCategory("Tasks", Entity.TASK));
        entityCategories.add(new UserItemEntityCategory("Tests", Entity.GHERKIN_TEST, Entity.MANUAL_TEST));
        entityCategories.add(new UserItemEntityCategory("Mention in comments", Entity.COMMENT));
        entityCategories.add(new UserItemEntityCategory("Runs", Entity.MANUAL_TEST_RUN , Entity.TEST_SUITE_RUN));
        EntityTreeModel model = new EntityTreeModel(entityCategories, entityModels);
        return model;
    }
}
