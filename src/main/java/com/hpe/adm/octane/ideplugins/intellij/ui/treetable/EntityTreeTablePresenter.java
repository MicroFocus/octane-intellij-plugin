package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.common.eventbus.EventBus;
import com.google.inject.Inject;
import com.google.inject.name.Named;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.OpenDetailTabEvent;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.RefreshMyWorkEvent;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.PartialEntity;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.intellij.util.RestUtil;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.MyWorkService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.nonentity.DownloadScriptService;
import com.hpe.adm.octane.ideplugins.services.util.SdkUtil;
import com.intellij.icons.AllIcons;
import com.intellij.ide.actions.OpenProjectFileChooserDescriptor;
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
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import static com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil.getUiDataFromModel;

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
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {

            @Override
            protected Void doInBackground() throws Exception {
                try {
                    entityTreeTableView.setLoading(true);
                    Collection<EntityModel> myWork = myWorkService.getMyWork(EntityTreeCellRenderer.getEntityFieldMap());
                    SwingUtilities.invokeLater(() -> {
                        entityTreeTableView.setLoading(false);
                        entityTreeTableView.setTreeModel(new EntityTreeModel(myWork));
                        entityTreeTableView.expandAllNodes();
                        updateActiveItem(myWork);
                    });
                    return null;
                } catch (Exception ex) {
                    entityTreeTableView.setLoading(false);
                    String message;
                    if (ex instanceof OctaneException) {
                        message = SdkUtil.getMessageFromOctaneException((OctaneException) ex);
                    } else {
                        message = ex.getMessage();
                    }
                    entityTreeTableView.setErrorMessage("Failed to load \"My work\" <br>" + message);
                    return null;
                }
            }
        };
        worker.execute();
    }

    /**
     * Clear active item if it's not in my work
     */
    private void updateActiveItem(Collection<EntityModel> myWork) {
        PartialEntity activeItem = getActiveItemFromPersistentState();
        if (activeItem != null && myWork != null) {
            List<EntityModel> matchedItems = myWork.stream()
                    .filter(entityModel -> activeItem.getEntityId() == Long.parseLong(entityModel.getValue("id").getValue().toString())
                            && activeItem.getEntityType() == Entity.getEntityType(entityModel))
                    .collect(Collectors.toList());
            if (!matchedItems.isEmpty()) {
                activeItem.setEntityName(matchedItems.get(0).getValue("name").getValue().toString());
                persistentState.saveState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM, PartialEntity.toJsonObject(activeItem));
            } else {
                persistentState.clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
            }
        } else {
            persistentState.clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
        }
    }

    public EntityTreeView getView() {
        return entityTreeTableView;
    }

    @Override
    @Inject
    public void setView(@Named("myWorkEntityTreeView") EntityTreeView entityTreeView) {
        this.entityTreeTableView = entityTreeView;

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

            if (entityType == Entity.TASK || entityType == Entity.COMMENT) {
                //Get parent info
                EntityModel parentEntityModel;
                if(entityType == Entity.TASK){
                    parentEntityModel = (EntityModel) entityModel.getValue("story").getValue();
                } else {
                    parentEntityModel = (EntityModel) UiUtil.getContainerItemForCommentModel(entityModel).getValue();
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

            if(myWorkService.isCurrentUserFollowing(entityModel)) {
                JMenuItem removeFromMyWorkMenuItem = new JMenuItem("Dismiss");
                removeFromMyWorkMenuItem.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        ApplicationManager.getApplication().invokeLater(() -> {
                            Task.Backgroundable backgroundTask =

                                    new Task.Backgroundable(
                                            null,
                                            "Dismissing item from to \"My Work\"",
                                            true) {

                                public void run(@NotNull ProgressIndicator indicator) {
                                    if(myWorkService.removeCurrentUserFromFollowers(entityModel)) {
                                        refresh();
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
}
