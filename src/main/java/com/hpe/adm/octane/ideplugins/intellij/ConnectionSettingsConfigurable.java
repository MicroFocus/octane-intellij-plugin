package com.hpe.adm.octane.ideplugins.intellij;

import com.hpe.adm.octane.ideplugins.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.panels.ConnectionSettingsView;
import com.hpe.adm.octane.ideplugins.intellij.util.UrlParser;
import com.hpe.adm.octane.ideplugins.services.ConnectionSettings;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.util.Comparing;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class ConnectionSettingsConfigurable implements SearchableConfigurable {

    public static final String NAME = "Octane";

    private ConnectionSettingsWrapper connectionSettingsWrapper = PluginModule.getInstance(ConnectionSettingsWrapper.class);
    private ConnectionSettingsView connectionSettingsView = PluginModule.getInstance(ConnectionSettingsView.class);

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
        connectionSettingsView.setServerUrl(connectionSettingsWrapper.getBaseUrl());
        connectionSettingsView.setUserName(connectionSettingsWrapper.getUserName());
        connectionSettingsView.setWorkspace(connectionSettingsWrapper.getWorkspaceId());
        connectionSettingsView.setPassword(connectionSettingsWrapper.getPassword());
        return connectionSettingsView.getRootPanel();
    }

    @Override
    public boolean isModified() {
        return connectionSettingsView != null
                && (!Comparing.equal(connectionSettingsWrapper.getBaseUrl(), connectionSettingsView.getServerUrl()))
                || (!Comparing.equal(connectionSettingsWrapper.getUserName(), connectionSettingsView.getUserName()))
                || (!Comparing.equal(connectionSettingsWrapper.getPassword(), connectionSettingsView.getUserName()));
    }

    @Override
    public void apply() throws ConfigurationException {
        if (null != connectionSettingsView) {

            //Parse server url
            ConnectionSettings connectionSettings = UrlParser.resolveConnectionSettings(
                    connectionSettingsView.getServerUrl(),
                    connectionSettingsView.getUserName(),
                    connectionSettingsView.getPassword());

            //Make view adjustments
            connectionSettingsView.setServerUrl(connectionSettings.getBaseUrl());
            connectionSettingsView.setWorkspace(connectionSettings.getWorkspaceId());

            //Modify the wrapper

            connectionSettingsWrapper.setBaseUrl(connectionSettings.getBaseUrl());

            connectionSettingsWrapper.setSharedSpaceId(connectionSettings.getSharedSpaceId());
            connectionSettingsWrapper.setWorkspaceId(connectionSettings.getWorkspaceId());

            connectionSettingsWrapper.setUserName(connectionSettingsView.getUserName());
            connectionSettingsWrapper.setPassword(connectionSettingsView.getPassword());
        }
    }

    @Override
    public void reset() {

    }

    @Override
    public void disposeUIResources() {
        connectionSettingsView = null;
    }


}
