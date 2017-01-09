package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.PartialEntity;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.openapi.actionSystem.*;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.*;
import java.util.Collection;

public class ToolbarActiveItem {

    private static EntityIconFactory entityIconFactory = new EntityIconFactory(20, 20, 10, Color.WHITE);

    private static ToolbarActiveItem instance;

    private AnAction activeItemAction;

    private DefaultActionGroup mainToolbarGroup;

    private IdePluginPersistentState persistentState;

    private static ImageIcon defectIcon = new ImageIcon(entityIconFactory.getIconAsImage(Entity.DEFECT));

    private static ImageIcon userStoryIcon = new ImageIcon(entityIconFactory.getIconAsImage(Entity.USER_STORY));

    public static ToolbarActiveItem getInstance() {
        if (instance == null)
            instance = new ToolbarActiveItem();
        return instance;
    }

    private ToolbarActiveItem() {

        mainToolbarGroup = (DefaultActionGroup) ActionManager.getInstance().
                getAction(IdeActions.GROUP_MAIN_TOOLBAR);
    }

    public void updateOnRefresh(Collection<EntityModel> myWork) {
        PartialEntity activeItem = getActiveItemFromPersistentState();
        if (activeItem != null) {
            boolean idFoundInMyWork = myWork.stream().anyMatch(
                    (entityModel) -> activeItem.getEntityId() == Long.parseLong(entityModel.getValue("id").getValue().toString()));
            if (idFoundInMyWork) {
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

    private static class ActiveItemAction extends AnAction {

        static int id;

        public ActiveItemAction(String text, String description, Icon icon) {
            super(text, description, icon);
            id++;
        }

        @Override
        public boolean displayTextInToolbar() {
            return true;
        }

        @Override
        public void actionPerformed(AnActionEvent e) {

        }
    }

    private static ActiveItemAction buildActionForItem(PartialEntity item) {
        String text = "#" + item.getEntityId() + ": " + item.getEntityName();
        ActiveItemAction action = new ActiveItemAction(text, text, item.getEntityType() == Entity.USER_STORY
                ? userStoryIcon : defectIcon);
        ActionManager.getInstance().registerAction("ActiveItemAction" + ActiveItemAction.id, action);
        return action;
    }

    public void changeItem() {
        PartialEntity newActiveItem = getActiveItemFromPersistentState();
        if (newActiveItem != null) {
            AnAction newAction = buildActionForItem(newActiveItem);
            if (activeItemAction == null) {
                mainToolbarGroup.addAction(newAction, Constraints.LAST);
            } else {
                mainToolbarGroup.replaceAction(activeItemAction, newAction);
            }
            activeItemAction = newAction;
        }
    }

    public void hideActiveItem() {
        if (activeItemAction != null) {
            mainToolbarGroup.remove(activeItemAction);
            activeItemAction = null;
        }
    }
}
