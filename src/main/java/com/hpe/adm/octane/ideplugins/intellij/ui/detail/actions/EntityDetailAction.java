package com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Optional;

public abstract class EntityDetailAction extends AnAction {

    public EntityDetailAction(@Nullable String text, @Nullable String description, @Nullable Icon icon){
        super(text, description, icon);
    }

    protected static Optional<EntityDetailPresenter> getCurrentEntityDetailPresenter(AnActionEvent e) {
        PluginModule pluginModule = PluginModule.getPluginModuleForProject(e.getProject());
        TabbedPanePresenter tabbedPanePresenter = pluginModule.getInstance(TabbedPanePresenter.class);
        return Optional.ofNullable(tabbedPanePresenter.getSelectedDetailTabPresenter());
    }

}
