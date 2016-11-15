package com.hpe.adm.octane.ideplugins.intellij.settings;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.panels.ConnectionSettingsView;
import com.hpe.adm.octane.ideplugins.intellij.util.UrlParser;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.util.Comparing;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class ConnectionSettingsConfigurable implements SearchableConfigurable {

    private static final String NAME = "Octane";

    private ConnectionSettingsProvider connectionSettingsProvider = PluginModule.getInstance(IdePersistentConnectionSettingsProvider.class);
    private ConnectionSettingsView connectionSettingsView = PluginModule.getInstance(ConnectionSettingsView.class);

    @NotNull
    @Override
    public String getId() {
        //TODO what is this supposed to be?
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
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
        connectionSettingsView.setServerUrl(connectionSettings.getBaseUrl());
        connectionSettingsView.setUserName(connectionSettings.getUserName());
        connectionSettingsView.setSharedspaceWorkspaceIds(connectionSettings.getWorkspaceId(), connectionSettings.getSharedSpaceId());
        connectionSettingsView.setPassword(connectionSettings.getPassword());
        return connectionSettingsView.getRootPanel();
    }

    @Override
    public boolean isModified() {

        ConnectionSettings currentConnectionSettings = connectionSettingsProvider.getConnectionSettings();

        ConnectionSettings viewConnectionSettings = UrlParser.resolveConnectionSettings(
                connectionSettingsView.getServerUrl(),
                connectionSettingsView.getUserName(),
                connectionSettingsView.getPassword());

        return !currentConnectionSettings.equals(viewConnectionSettings);

    }

    @Override
    public void apply() throws ConfigurationException {
        if (null != connectionSettingsView) {
            //Parse server url
            ConnectionSettings connectionSettings = UrlParser.resolveConnectionSettings(
                    connectionSettingsView.getServerUrl(),
                    connectionSettingsView.getUserName(),
                    connectionSettingsView.getPassword());

            //Modify the provider
            connectionSettingsProvider.setConnectionSettings(connectionSettings);
        }
    }

    @Override
    public void reset() {
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
        connectionSettingsView.setServerUrl(connectionSettings.getBaseUrl());
        connectionSettingsView.setSharedspaceWorkspaceIds(connectionSettings.getSharedSpaceId(), connectionSettings.getWorkspaceId());
        connectionSettingsView.setUserName(connectionSettings.getUserName());
        connectionSettingsView.setPassword(connectionSettings.getUserName());
    }

    @Override
    public void disposeUIResources() {
        connectionSettingsView = null;
    }


}
