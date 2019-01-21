package com.hpe.adm.octane.ideplugins.intellij.actions.activeitem;

import com.hpe.adm.octane.ideplugins.intellij.actions.OctanePluginAction;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import org.jetbrains.annotations.NotNull;
import org.json.JSONObject;

import javax.swing.*;
import java.util.Objects;

public class OpenActiveItemAction extends OctanePluginAction {

    private PartialEntity prevActiveItem;

    public OpenActiveItemAction() {
        super("Open active backlog item", "Open a detail tab with the active backlog item.", IconLoader.findIcon(Constants.IMG_REFRESH_ICON));
    }

    @Override
    public void update(@NotNull AnActionEvent e) {
        if(e.getProject() == null) {
            return;
        }


        getPluginModule(e).ifPresent(pluginModule -> {

            JSONObject jsonObject = pluginModule.getInstance(IdePluginPersistentState.class).loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);

            e.getPresentation().setEnabled(false);

            if (jsonObject != null) {
                PartialEntity activeItem = PartialEntity.fromJsonObject(jsonObject);

                // This is necessary to avoid doing a rest call for labels on init
                if(!Objects.equals(activeItem, prevActiveItem)) {
                    prevActiveItem = activeItem;

                    e.getPresentation().setDescription(activeItem.getEntityName());
                    e.getPresentation().setText("#" + activeItem.getEntityId());

                    // Has to be in a thread other than the UI, because the EntityIconFactory will trigger sso login on startup
                    // The thread is not that expensive because of the presentation is only updated if the active item changed
                    new Thread(() -> e.getPresentation().setIcon(
                            new ImageIcon(pluginModule
                                    .getInstance(EntityIconFactory.class)
                                    .getIconAsImage(activeItem.getEntityType(), 20, 11
                                    )))).start();

                    e.getPresentation().setEnabled(true);
                }
            }
        });
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        if(e.getProject() == null) {
            return;
        }

        getPluginModule(e).ifPresent(pluginModule -> {
            JSONObject jsonObject = pluginModule.getInstance(IdePluginPersistentState.class).loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
            if (jsonObject != null) {
                ToolWindow octaneToolWindow = ToolWindowManager.getInstance(Objects.requireNonNull(e.getProject())).getToolWindow("ALM Octane");
                if (!octaneToolWindow.isActive()) {
                    ToolWindowManager.getInstance(e.getProject()).getToolWindow("ALM Octane").show(null);
                }
                TabbedPanePresenter tabbedPanePresenter = pluginModule.getInstance(TabbedPanePresenter.class);
                tabbedPanePresenter.openDetailTab(PartialEntity.fromJsonObject(jsonObject));
            }
        });
    }

}