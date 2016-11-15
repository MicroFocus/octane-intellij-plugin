package com.hpe.adm.octane.ideplugins.intellij;

import com.hpe.adm.octane.ideplugins.PluginModuleManager;
import com.hpe.adm.octane.ideplugins.intellij.ui.panels.ConnectionSettingsView;
import com.hpe.adm.octane.ideplugins.services.ConnectionSettings;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class ConnectionSettingsConfigurable implements SearchableConfigurable {

    public static final String NAME = "Octane";
    private ConnectionSettingsWrapper connectionSettingsWrapper;
    private ConnectionSettingsView connectionSettingsView;
    private ConnectionSettings connectionSettings = ConnectionSettings.getInstance();

    @NotNull
    @Override
    public String getId() {
        return null;
    }

    @Nullable
    @Override
    public Runnable enableSearch(String option) {
        return null;
    }

    @Nls
    @Override
    public String getDisplayName() {
        return NAME;
    }

    @Nullable
    @Override
    public String getHelpTopic() {
        return "settings.octane";
    }

    @Nullable
    @Override
    public JComponent createComponent() {
        if (connectionSettingsView == null) {
            connectionSettingsView = PluginModuleManager.getInstance(ConnectionSettingsView.class);
//            connectionSettingsView = new ConnectionSettingsView();
        }
        return connectionSettingsView.getRootPanel();
    }

    @Override
    public boolean isModified() {
//        return connectionSettingsView != null
//                && (!Comparing.equal(connectionSettings.getUserName(), connectionSettingsView.getUserName())
//                || isPasswordModified())
//                || !Comparing.equal(connectionSettings.getBaseUrl(), connectionSettingsView.getBaseUrl());
        return true;
    }

    @Override
    public void apply() throws ConfigurationException {

    }

    @Override
    public void reset() {

    }

    @Override
    public void disposeUIResources() {
        connectionSettingsView = null;
    }


}
