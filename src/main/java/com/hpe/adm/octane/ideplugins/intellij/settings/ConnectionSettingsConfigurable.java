package com.hpe.adm.octane.ideplugins.intellij.settings;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.components.ConnectionSettingsComponent;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.util.UrlParser;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class ConnectionSettingsConfigurable implements SearchableConfigurable {

    private static final String NAME = "Octane";

    private static final String CONNECTION_FAILED_DIALOG_MESSAGE = "Failed to connect with given connection settings";
    private static final String CONNECTION_SUCCESSFUL_DIALOG_MESSAGE = "Connection successful";
    private static final String URL_PARSE_FAILED_DIALOG_MESSAGE = "Failed to parse given server URL, bad format";
    private static final String CONNECTION_DIALOG_TITLE = "Connection status";

    //@Inject is not supported here, this class is instantiated by intellij
    private ConnectionSettingsProvider connectionSettingsProvider = PluginModule.getInstance(ConnectionSettingsProvider.class);
    private ConnectionSettingsComponent connectionSettingsView = new ConnectionSettingsComponent();

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

        connectionSettingsView.setTestConnectionActionListener(event -> {
            try {
                testConnection();
            } catch (ConfigurationException ex){
                //testConnection sets the labels that's all we care about
            }
        });

        return connectionSettingsView.getComponent();
    }

    @Override
    public boolean isModified() {

        ConnectionSettings currentConnectionSettings = connectionSettingsProvider.getConnectionSettings();

        ConnectionSettings viewConnectionSettings = null;
        try {
            viewConnectionSettings = UrlParser.resolveConnectionSettings(
                    connectionSettingsView.getServerUrl(),
                    connectionSettingsView.getUserName(),
                    connectionSettingsView.getPassword());
        } catch (ServiceException e) {
            viewConnectionSettings = new ConnectionSettings();
        }

        return !viewConnectionSettings.equals(currentConnectionSettings);
    }

    @Override
    public void apply() throws ConfigurationException {
        //Throws ConfigurationException in case it fails
        testConnection();

        //apply otherwise
        try {
            connectionSettingsProvider.setConnectionSettings(getConnectionSettingsFromView());
        } catch (ServiceException ex){
            //can never happen, testConnection does the same check b4
        }
    }

    private void testConnection(ConnectionSettings connectionSettings) throws ConfigurationException {
        try {
            //Clear previous message
            connectionSettingsView.setConnectionStatusLabel(false);

            //throws config exception if invalid
            ConnectionSettings newConnectionSettings = getConnectionSettingsFromView();

            //Will use the new provider
            TestService testService = PluginModule.getInstance(TestService.class);
            testService.testConnection(connectionSettings);

        } catch(Exception ex){
            connectionSettingsView.setConnectionStatusErrorLabel(ex.getMessage());
            throw new ConfigurationException(CONNECTION_FAILED_DIALOG_MESSAGE + ": \r\n" + ex.getMessage());
        }
        connectionSettingsView.setConnectionStatusSuccessLabel();
    }

    private ConnectionSettings getConnectionSettingsFromView() throws ServiceException{
        //Parse server url
        return UrlParser.resolveConnectionSettings(
                connectionSettingsView.getServerUrl(),
                connectionSettingsView.getUserName(),
                connectionSettingsView.getPassword());
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
