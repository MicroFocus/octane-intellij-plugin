package com.hpe.adm.octane.ideplugins.intellij.actions;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Optional;

public abstract class OctanePluginAction extends AnAction {

    public OctanePluginAction(@Nullable String text, @Nullable String description, @Nullable Icon icon){
        super(text, description, icon);
    }

    protected static Presenter getSelectedPresenter(AnActionEvent e) {
        PluginModule pluginModule = PluginModule.getPluginModuleForProject(e.getProject());
        TabbedPanePresenter tabbedPanePresenter = pluginModule.getInstance(TabbedPanePresenter.class);
        return tabbedPanePresenter.getSelectedPresenter();
    }

    protected static Optional<PluginModule> getPluginModule(AnActionEvent e) {
        return e.getProject() == null ? Optional.empty() : Optional.of(PluginModule.getPluginModuleForProject(e.getProject()));
    }

}