package com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

public final class SaveAction extends AnAction {
    public SaveAction() {
        super("Save current entity", "this will save the new phase entity", IconLoader.findIcon("/actions/menu-saveall.png"));
    }

    public void actionPerformed(AnActionEvent e) {

    }
}
