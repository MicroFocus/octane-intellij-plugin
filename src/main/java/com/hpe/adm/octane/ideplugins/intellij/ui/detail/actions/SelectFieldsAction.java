package com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions;

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailView;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

public final class SelectFieldsAction extends AnAction {

    private boolean defaultfields = true;
    private EntityDetailView entityDetailView;

    public void setDefaultFieldsIcon(boolean defaultfields) {
        this.defaultfields = defaultfields;
    }

    public SelectFieldsAction(EntityDetailView entityDetailView) {
        super("Select fields for this entity type", "Select fields popup", IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_DEFAULT));
        this.entityDetailView = entityDetailView;
    }

    public void actionPerformed(AnActionEvent e) {
        entityDetailView.getEntityDetailsPanel().activateFieldsSettings();
    }

    public void update(AnActionEvent e) {
        if (defaultfields) {
            e.getPresentation().setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_DEFAULT));
        } else {
            e.getPresentation().setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_NON_DEFAULT));
        }
    }
}
