package com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

public class RefreshCurrentEntityAction extends AnAction {

    public RefreshCurrentEntityAction() {
        super("Refresh backlog item", "Refresh backlog item details.", IconLoader.findIcon(Constants.IMG_REFRESH_ICON));
    }

    public void actionPerformed(AnActionEvent e) {
        EntityDetailPresenter entityDetailPresenter = getCurrentEntityDetailPresenter(e);
        if (entityDetailPresenter != null) {
            entityDetailPresenter.refreshEntity();
        }
    }

    private static EntityDetailPresenter getCurrentEntityDetailPresenter(AnActionEvent e) {
        PluginModule pluginModule = PluginModule.getPluginModuleForProject(e.getProject());
        TabbedPanePresenter tabbedPanePresenter = pluginModule.getInstance(TabbedPanePresenter.class);
        return tabbedPanePresenter.getSelectedDetailTabPresenter();
    }

}