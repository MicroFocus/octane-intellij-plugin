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

package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.gitcommit.CommitMessageUtils;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.project.ProjectManagerListener;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.util.HashMap;
import java.util.Map;

public class ToolbarActiveItem {

    private static EntityIconFactory entityIconFactory = new EntityIconFactory(20, 20, 10, Color.WHITE);
    private static Map<Project, Runnable> activeItemClickHandlers = new HashMap<>();
    private ActiveItemAction activeItemAction;
    private CopyCommitMessageAction copyCommitMessageAction;
    private StopActiveItemAction stopActiveItemAction;

    private IdePluginPersistentState persistentState;
    private Project project;
    private boolean shouldUpdatePresentation = true; //true for initial draw

    private class ActiveItemAction extends AnAction {

        PartialEntity partialEntity;

        public ActiveItemAction(PartialEntity partialEntity) {
            this.partialEntity = partialEntity;
        }

        public void setPartialEntity(PartialEntity partialEntity) {
            this.partialEntity = partialEntity;
            shouldUpdatePresentation = true;
        }

        @Override
        public boolean displayTextInToolbar() {
            return true;
        }

        @Override
        public void update(AnActionEvent e) {
            Project eventProject = e.getDataContext().getData(CommonDataKeys.PROJECT);

            boolean isVisible = partialEntity != null;

            //Compare update actions source to the DI project
            //This is to not show the active item of another project in the same IntelliJ toolbar
            if (eventProject != null && !eventProject.equals(ToolbarActiveItem.this.project)) {
                isVisible = false;
            }

            //Update visibility
            e.getPresentation().setVisible(isVisible);

            //Only update the presentation when it's actually needed, to avoid spamming ImageIcon objects that need to be gc later
            if (shouldUpdatePresentation && isVisible) {
                updatePresentation(e.getPresentation());
                shouldUpdatePresentation = false;
            }
        }

        private void updatePresentation(Presentation presentation) {
            presentation.setDescription(partialEntity.getEntityName());
            presentation.setText("");
            presentation.setText("#" + partialEntity.getEntityId());
            presentation.setIcon(new ImageIcon(entityIconFactory.getIconAsImage(partialEntity.getEntityType())));
        }

        @Override
        public void actionPerformed(AnActionEvent e) {
            Project project = DataKeys.PROJECT.getData(e.getDataContext());
            if (activeItemClickHandlers.containsKey(project)) {
                activeItemClickHandlers.get(project).run();
            }
            ToolWindow octaneToolWindow = ToolWindowManager.getInstance(project).getToolWindow("ALM Octane");
            if (!octaneToolWindow.isActive()) {
                ToolWindowManager.getInstance(project).getToolWindow("ALM Octane").show(() -> {
                });
            }
        }
    }

    private class CopyCommitMessageAction extends AnAction {

        PartialEntity partialEntity;
        CommitMessageUtils commitMessageUtils;

        public CopyCommitMessageAction(PartialEntity partialEntity) {
            super("Copy commit message for active item", "Copies commit message for active item", IconLoader.findIcon(Constants.IMG_COPY_ICON));
            this.partialEntity = partialEntity;
            PluginModule pluginModule = PluginModule.getPluginModuleForProject(project);
            commitMessageUtils = pluginModule.getInstance(CommitMessageUtils.class);
        }

        public void setPartialEntity(PartialEntity partialEntity) {
            this.partialEntity = partialEntity;
        }

        @Override
        public void update(AnActionEvent e) {
            Project eventProject = e.getDataContext().getData(CommonDataKeys.PROJECT);

            boolean isVisible = partialEntity != null;

            //Compare update actions source to the DI project
            //This is to not show the active item of another project in the same IntelliJ toolbar
            if (eventProject != null && !eventProject.equals(ToolbarActiveItem.this.project)) {
                isVisible = false;
            }

            //Update visibility
            e.getPresentation().setVisible(isVisible);
        }


        @Override
        public void actionPerformed(AnActionEvent e) {
            StringSelection selection = new StringSelection(commitMessageUtils.getCommitMessage(partialEntity));
            Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
            clipboard.setContents(selection, selection);
            commitMessageUtils.showCommitPatterns(partialEntity);
        }
    }

    private class StopActiveItemAction extends AnAction {

        PartialEntity partialEntity;
        CommitMessageUtils commitMessageUtils;

        public StopActiveItemAction(PartialEntity partialEntity) {
            super("Stop work on the active item", "Stops work on active item", IconLoader.findIcon(Constants.IMG_STOP_TIMER));
            this.partialEntity = partialEntity;
            PluginModule pluginModule = PluginModule.getPluginModuleForProject(project);
            commitMessageUtils = pluginModule.getInstance(CommitMessageUtils.class);
        }

        public void setPartialEntity(PartialEntity partialEntity) {
            this.partialEntity = partialEntity;
        }

        @Override
        public void update(AnActionEvent e) {
            Project eventProject = e.getDataContext().getData(CommonDataKeys.PROJECT);

            boolean isVisible = partialEntity != null;

            //Compare update actions source to the DI project
            //This is to not show the active item of another project in the same IntelliJ toolbar
            if (eventProject != null && !eventProject.equals(ToolbarActiveItem.this.project)) {
                isVisible = false;
            }

            //Update visibility
            e.getPresentation().setVisible(isVisible);
        }


        @Override
        public void actionPerformed(AnActionEvent e) {
            persistentState.clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
        }
    }

    @Inject
    public ToolbarActiveItem(IdePluginPersistentState persistentState, Project project) {
        this.persistentState = persistentState;
        this.project = project;

        activeItemAction = new ActiveItemAction(getActiveItemFromPersistentState());
        copyCommitMessageAction = new CopyCommitMessageAction(getActiveItemFromPersistentState());
        stopActiveItemAction = new StopActiveItemAction(getActiveItemFromPersistentState());

        DefaultActionGroup defaultActionGroup = (DefaultActionGroup) ActionManager
                .getInstance()
                .getAction(
                        "ToolbarRunGroup");
        DefaultActionGroup activeItemActionGroup = new DefaultActionGroup();

        Separator first = Separator.create();
        activeItemActionGroup.add(first, Constraints.FIRST);
        activeItemActionGroup.add(stopActiveItemAction, Constraints.FIRST);
        activeItemActionGroup.add(copyCommitMessageAction, Constraints.FIRST);
        activeItemActionGroup.add(activeItemAction, Constraints.FIRST);

        defaultActionGroup.add(activeItemActionGroup, Constraints.FIRST);

        persistentState.addStateChangedHandler((key, value) -> {
            if (key == IdePluginPersistentState.Key.ACTIVE_WORK_ITEM) {
                PartialEntity newActiveItem = getActiveItemFromPersistentState();
                activeItemAction.setPartialEntity(newActiveItem);
                copyCommitMessageAction.setPartialEntity(newActiveItem);
                stopActiveItemAction.setPartialEntity(newActiveItem);
            }
        });

        ProjectManager.getInstance().addProjectManagerListener(project, new ProjectManagerListener() {
            @Override
            public void projectClosing(Project project) {
                if (project.equals(ToolbarActiveItem.this.project)) {
                    defaultActionGroup.remove(activeItemActionGroup);
                    activeItemClickHandlers.remove(project);
                }
            }
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

    public static void setActiveItemClickHandler(Project project, Runnable runnable) {
        activeItemClickHandlers.put(project, runnable);
    }
}
