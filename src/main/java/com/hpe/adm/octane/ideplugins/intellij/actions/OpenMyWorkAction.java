package com.hpe.adm.octane.ideplugins.intellij.actions;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;

public class OpenMyWorkAction extends AnAction {

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
        tabbedPanePresenter.selectMyWorkTab();
    }

}