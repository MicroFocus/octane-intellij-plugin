package com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

import java.util.Random;

public final class SaveCurrentEntityAction extends AnAction {

    public SaveCurrentEntityAction() {
        super("Save backlog item", "Save changes to backlog item.", IconLoader.findIcon("/actions/menu-saveall.png"));
        getTemplatePresentation().setEnabled(false);
    }

    public void update(AnActionEvent e) {
        EntityDetailPresenter entityDetailPresenter = getCurrentEntityDetailPresenter(e);
        //boolean isEnabled = entityDetailPresenter != null && entityDetailPresenter.wasEntityChanged();
        boolean isEnabled = new Random().nextBoolean();
        getTemplatePresentation().setEnabled(isEnabled);
    }

    public void actionPerformed(AnActionEvent e) {
        EntityDetailPresenter entityDetailPresenter = getCurrentEntityDetailPresenter(e);
        if (entityDetailPresenter != null) {
            entityDetailPresenter.saveEntity();
        }
    }

    private static EntityDetailPresenter getCurrentEntityDetailPresenter(AnActionEvent e) {
        PluginModule pluginModule = PluginModule.getPluginModuleForProject(e.getProject());
        TabbedPanePresenter tabbedPanePresenter = pluginModule.getInstance(TabbedPanePresenter.class);
        return tabbedPanePresenter.getSelectedDetailTabPresenter();
    }

}