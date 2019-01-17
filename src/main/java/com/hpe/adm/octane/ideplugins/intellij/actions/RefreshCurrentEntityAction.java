package com.hpe.adm.octane.ideplugins.intellij.actions;

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

public class RefreshCurrentEntityAction extends OctanePluginAction {

    public RefreshCurrentEntityAction() {
        super("Refresh backlog item", "Refresh backlog item details.", IconLoader.findIcon(Constants.IMG_REFRESH_ICON));
    }

    public void actionPerformed(AnActionEvent e) {
        getCurrentEntityDetailPresenter(e).ifPresent(EntityDetailPresenter::refreshEntity);
    }

}