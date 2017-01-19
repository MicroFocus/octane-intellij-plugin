package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.PartialEntity;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

public class ToolbarActiveItem {

    private static EntityIconFactory entityIconFactory = new EntityIconFactory(20, 20, 10, Color.WHITE);
    private static Map<Project, Runnable> activeItemClickHandlers = new HashMap<>();
    private ActiveItemAction activeItemAction;

    private IdePluginPersistentState persistentState;
    private Project project;

    private class ActiveItemAction extends AnAction {

        PartialEntity partialEntity;

        public ActiveItemAction(PartialEntity partialEntity) {
            this.partialEntity = partialEntity;
        }

        public void setPartialEntity(PartialEntity partialEntity) {
            this.partialEntity = partialEntity;
        }

        @Override
        public boolean displayTextInToolbar() {
            return true;
        }

        @Override
        public void update(AnActionEvent e) {
            Project eventProject = e.getDataContext().getData(CommonDataKeys.PROJECT);
            //Compare update actions source to the DI project
            if(eventProject!=null && !eventProject.equals(ToolbarActiveItem.this.project)){
                e.getPresentation().setVisible(false);
            } else {
                if(partialEntity!=null) {
                    e.getPresentation().setVisible(true);
                    e.getPresentation().setText("#" + partialEntity.getEntityId());
                    e.getPresentation().setDescription(partialEntity.getEntityName());
                    e.getPresentation().setIcon(new ImageIcon(entityIconFactory.getIconAsImage(partialEntity.getEntityType())));
                } else {
                    e.getPresentation().setVisible(false);
                }
            }
        }

        @Override
        public void actionPerformed(AnActionEvent e) {
            Project project = DataKeys.PROJECT.getData(e.getDataContext());
            if(activeItemClickHandlers.containsKey(project)){
                activeItemClickHandlers.get(project).run();
            }
            ToolWindow octaneToolWindow = ToolWindowManager.getInstance(project).getToolWindow("ALM Octane");
            if (!octaneToolWindow.isActive()) {
                ToolWindowManager.getInstance(project).getToolWindow("ALM Octane").show(() -> {
                });
            }
        }
    }

    @Inject
    public ToolbarActiveItem(IdePluginPersistentState persistentState, Project project){
        this.persistentState = persistentState;
        this.project = project;

        activeItemAction = new ActiveItemAction(getActiveItemFromPersistentState());

        persistentState.addStateChangedHandler((key, value) -> {
            if(key == IdePluginPersistentState.Key.ACTIVE_WORK_ITEM){
                activeItemAction.setPartialEntity(getActiveItemFromPersistentState());
            }
        });

        DefaultActionGroup defaultActionGroup = (DefaultActionGroup) ActionManager.getInstance().getAction(
                "ToolbarRunGroup");
        defaultActionGroup.add(activeItemAction, Constraints.FIRST);

        //Text color fix on LAF change
        UIManager.addPropertyChangeListener(evt -> {
            if(evt.getPropertyName().equals("lookAndFeel")) {
                defaultActionGroup.remove(activeItemAction);
                activeItemAction = new ActiveItemAction(getActiveItemFromPersistentState());
                defaultActionGroup.add(activeItemAction, Constraints.FIRST);
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

    public static void setActiveItemClickHandler(Project project, Runnable runnable){
        activeItemClickHandlers.put(project, runnable);
    }
}
