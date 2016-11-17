package com.hpe.adm.octane.ideplugins.intellij.settings;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.panels.ConnectionSettingsView;
import com.hpe.adm.octane.ideplugins.intellij.util.UrlParser;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class ConnectionSettingsConfigurable implements SearchableConfigurable {

    private static final String NAME = "Octane";

    private static final String CONNECTION_FAILED_DIALOG_MESSAGE = "Failed to connect with given connection settings";
    private static final String URL_PARSE_FAILED_DIALOG_MESSAGE = "Failed to parse given server URL, bad format.";
    private static final String CONNECTION_DIALOG_TITLE = "Connection status";

    //@Inject is not supported here, this class is instantiated by intellij
    private ConnectionSettingsProvider connectionSettingsProvider = PluginModule.getInstance(ConnectionSettingsProvider.class);
    private ConnectionSettingsView connectionSettingsView = PluginModule.getInstance(ConnectionSettingsView.class);

    @NotNull
    @Override
    public String getId() {
        return "settings.octane";
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

        //Setting the base url will fire the even handler in the view, this will set the shared space and workspace fields
        connectionSettingsView.setServerUrl(UrlParser.createUrlFromConnectionSettings(connectionSettings));

        connectionSettingsView.setUserName(connectionSettings.getUserName());
        connectionSettingsView.setPassword(connectionSettings.getPassword());

        //Test connection should now do apply
        connectionSettingsView.setTestConnectionActionListener(event -> {
            try {
                ConnectionSettingsConfigurable.this.apply();
                Messages.showInfoMessage("Connection successful", CONNECTION_DIALOG_TITLE);
            } catch (ConfigurationException ex){
                Messages.showErrorDialog(CONNECTION_FAILED_DIALOG_MESSAGE, CONNECTION_DIALOG_TITLE);
            }
        });

        return connectionSettingsView.getRootPanel();
    }

    @Override
    public boolean isModified() {

        ConnectionSettings currentConnectionSettings = connectionSettingsProvider.getConnectionSettings();

        ConnectionSettings viewConnectionSettings = UrlParser.resolveConnectionSettings(
                connectionSettingsView.getServerUrl(),
                connectionSettingsView.getUserName(),
                connectionSettingsView.getPassword());

        return !viewConnectionSettings.equals(currentConnectionSettings);
    }

    @Override
    public void apply() throws ConfigurationException {
        //Parse server url
        ConnectionSettings newConnectionSettings = UrlParser.resolveConnectionSettings(
                connectionSettingsView.getServerUrl(),
                connectionSettingsView.getUserName(),
                connectionSettingsView.getPassword());

        if(newConnectionSettings.getBaseUrl() == null ||
                newConnectionSettings.getWorkspaceId() == null ||
                newConnectionSettings.getSharedSpaceId() == null) {
            throw new ConfigurationException(URL_PARSE_FAILED_DIALOG_MESSAGE);
        }

        //Modify the provider
        connectionSettingsProvider.setConnectionSettings(newConnectionSettings);

        //Will use the new provider
        TestService testService = PluginModule.getInstance(TestService.class);

        try{
            testService.testConnection();
        } catch (Exception ex){
            throw new ConfigurationException(CONNECTION_FAILED_DIALOG_MESSAGE);
        }
    }

    @Override
    public void reset() {
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
        connectionSettingsView.setServerUrl(UrlParser.createUrlFromConnectionSettings(connectionSettings));
        connectionSettingsView.setUserName(connectionSettings.getUserName());
        connectionSettingsView.setPassword(connectionSettings.getPassword());
    }

    @Override
    public void disposeUIResources() {
        connectionSettingsView = null;
    }

}
