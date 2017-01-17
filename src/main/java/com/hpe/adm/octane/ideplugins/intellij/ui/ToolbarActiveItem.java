package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.PartialEntity;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.project.Project;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.*;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ToolbarActiveItem {

    private static class ActiveItemAction extends AnAction {
        public ActiveItemAction(String text, String description, Icon icon) {
            super(text, description, icon);
        }

        @Override
        public boolean displayTextInToolbar() {
            return true;
        }

        @Override
        public void actionPerformed(AnActionEvent e) {
            Project project = DataKeys.PROJECT.getData(e.getDataContext());
            if(activeItemClickHandlers.containsKey(project)){
                activeItemClickHandlers.get(project).run();
            }
        }
    }

    private static EntityIconFactory entityIconFactory = new EntityIconFactory(20, 20, 10, Color.WHITE);
    private static Map<Project, Runnable> activeItemClickHandlers = new HashMap<>();
    private static ImageIcon defectIcon = new ImageIcon(entityIconFactory.getIconAsImage(Entity.DEFECT));
    private static ImageIcon userStoryIcon = new ImageIcon(entityIconFactory.getIconAsImage(Entity.USER_STORY));
    private static ImageIcon qualityStoryIcon = new ImageIcon(entityIconFactory.getIconAsImage(Entity.QUALITY_STORY));

    private AnAction activeItemAction;

    @Inject
    private IdePluginPersistentState persistentState;


    private static DefaultActionGroup getMenuBarActionGroup(){
        return (DefaultActionGroup) ActionManager.getInstance().getAction(IdeActions.GROUP_MAIN_TOOLBAR);
    }

    public void update(Collection<EntityModel> myWork) {
        PartialEntity activeItem = getActiveItemFromPersistentState();
        if (activeItem != null && myWork != null) {
            List<EntityModel> matchedItems = myWork.stream()
                    .filter(entityModel -> activeItem.getEntityId() == Long.parseLong(entityModel.getValue("id").getValue().toString()))
                    .collect(Collectors.toList());
            if (!matchedItems.isEmpty()) {
                activeItem.setEntityName(matchedItems.get(0).getValue("name").getValue().toString());
                persistentState.saveState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM, PartialEntity.toJsonObject(activeItem));
                changeItem();
            } else {
                persistentState.clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
                hideActiveItem();
            }
        } else {
            hideActiveItem();
        }
    }

    public void setPersistentState(IdePluginPersistentState persistentState) {
        this.persistentState = persistentState;
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

    private static ActiveItemAction buildActionForItem(PartialEntity item) {
        String text = "#" + item.getEntityId() + ": " + item.getEntityName();

        ImageIcon itemIcon = null;
        switch (item.getEntityType()) {
            case USER_STORY:
                itemIcon = userStoryIcon;
                break;
            case QUALITY_STORY:
                itemIcon = qualityStoryIcon;
                break;
            case DEFECT:
                itemIcon = defectIcon;
                break;
        }

        ActiveItemAction action = new ActiveItemAction(text, text, itemIcon);
        //ActionManager.getInstance().registerAction("ActiveItemAction" + ActiveItemAction.id, action);
        return action;
    }

    public void changeItem() {
        PartialEntity newActiveItem = getActiveItemFromPersistentState();
        if (newActiveItem != null) {
            AnAction newAction = buildActionForItem(newActiveItem);
            if (activeItemAction == null) {
                getMenuBarActionGroup().addAction(newAction, Constraints.LAST);
            } else {
                getMenuBarActionGroup().replaceAction(activeItemAction, newAction);
            }
            activeItemAction = newAction;
        }
    }

    public void hideActiveItem() {
        if (activeItemAction != null) {
            getMenuBarActionGroup().remove(activeItemAction);
            activeItemAction = null;
        }
    }
}
