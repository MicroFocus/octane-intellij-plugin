package com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions;

import com.hpe.adm.octane.ideplugins.intellij.actions.OctanePluginAction;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

public final class SaveCurrentEntityAction extends OctanePluginAction {

    public SaveCurrentEntityAction() {
        super("Save backlog item", "Save changes to backlog item.", IconLoader.findIcon("/actions/menu-saveall.png"));
        getTemplatePresentation().setEnabled(false);
    }

    public void update(AnActionEvent e) {
        Presenter presenter = getSelectedPresenter(e);
        if(presenter instanceof EntityDetailPresenter) {
            e.getPresentation().setEnabled(((EntityDetailPresenter) presenter).wasEntityChanged());
        }
    }

    public void actionPerformed(AnActionEvent e) {
        Presenter presenter = getSelectedPresenter(e);
        if(presenter instanceof EntityDetailPresenter) {
            ((EntityDetailPresenter) presenter).saveEntity();
        }
    }

}