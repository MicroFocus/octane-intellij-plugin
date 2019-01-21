package com.hpe.adm.octane.ideplugins.intellij.actions;

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

public class RefreshCurrentEntityAction extends OctanePluginAction {

    public RefreshCurrentEntityAction() {
        super("Refresh backlog item", "Refresh backlog item details.", IconLoader.findIcon(Constants.IMG_REFRESH_ICON));
    }

    public void actionPerformed(AnActionEvent e) {
        Presenter presenter = getSelectedPresenter(e);

        if(presenter == null) {
            return;
        }

        if(presenter instanceof EntityTreeTablePresenter) {
            ((EntityTreeTablePresenter) presenter).refresh();
        }
        else if (presenter instanceof EntityDetailPresenter) {
            ((EntityDetailPresenter) presenter).refresh();
        }

    }
}