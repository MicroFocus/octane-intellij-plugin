package com.hpe.adm.octane.ideplugins.intellij.actions;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import org.json.JSONObject;

public class OpenActiveItemAction extends AnAction {

    public OpenActiveItemAction() {
        super("Open active backlog item", "Open a detail tab with the active backlog item.", IconLoader.findIcon(Constants.IMG_REFRESH_ICON));
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        if(e.getProject() == null) {
            return;
        }

        ToolWindow octaneToolWindow = ToolWindowManager.getInstance(e.getProject()).getToolWindow("ALM Octane");
        if (!octaneToolWindow.isActive()) {
            ToolWindowManager.getInstance(e.getProject()).getToolWindow("ALM Octane").show(null);
        }

        PluginModule pluginModule = PluginModule.getPluginModuleForProject(e.getProject());
        TabbedPanePresenter tabbedPanePresenter = pluginModule.getInstance(TabbedPanePresenter.class);
        tabbedPanePresenter.openOrSelectActiveItem();
    }

}