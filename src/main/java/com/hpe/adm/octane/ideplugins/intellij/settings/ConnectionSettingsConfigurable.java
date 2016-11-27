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
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

import static com.hpe.adm.octane.ideplugins.services.util.UrlParser.resolveConnectionSettings;

public class ConnectionSettingsConfigurable implements SearchableConfigurable {

    private static final String NAME = "Octane";

    //@Inject is not supported here, this class is instantiated by intellij
    private ConnectionSettingsProvider connectionSettingsProvider = PluginModule.getInstance(ConnectionSettingsProvider.class);
    private ConnectionSettingsComponent connectionSettingsView = new ConnectionSettingsComponent();
    private TestService testService = PluginModule.getInstance(TestService.class);

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
            //Clear previous message
            connectionSettingsView.setConnectionStatusLoading();

            new SwingWorker() {
                @Override
                protected Void doInBackground() throws Exception {
                    try {
                        testService.testConnection(getConnectionSettingsFromView());
                        SwingUtilities.invokeLater(connectionSettingsView::setConnectionStatusSuccess);
                    } catch (ServiceException ex){
                        SwingUtilities.invokeLater(() ->  connectionSettingsView.setConnectionStatusError(ex.getMessage()));
                    }
                    return null;
                }
            }.execute();

        });

        return connectionSettingsView.getComponent();
    }

    @Override
    public boolean isModified() {

        ConnectionSettings currentConnectionSettings = connectionSettingsProvider.getConnectionSettings();

        ConnectionSettings viewConnectionSettings;
        try {
            viewConnectionSettings = resolveConnectionSettings(
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
        //If the connection settings are empty then save them, only way to clear and save
        if(isViewConnectionSettingsEmpty()){
            connectionSettingsProvider.setConnectionSettings(new ConnectionSettings());
            return;
        }

        ConnectionSettings newConnectionSettings;
        try {
            newConnectionSettings = getConnectionSettingsFromView();
            testService.testConnection(newConnectionSettings);
        } catch (ServiceException ex) {
            connectionSettingsView.setConnectionStatusError(ex.getMessage());
            return;
        }

        //apply if valid
        connectionSettingsProvider.setConnectionSettings(newConnectionSettings);
        //remove the hash and remove extra stuff if successful
        SwingUtilities.invokeLater(() -> connectionSettingsView.setServerUrl(UrlParser.createUrlFromConnectionSettings(newConnectionSettings)));
        connectionSettingsView.setConnectionStatusSuccess();
    }


    private ConnectionSettings getConnectionSettingsFromView() throws ServiceException{
        //Parse server url
        ConnectionSettings connectionSettings = UrlParser.resolveConnectionSettings(
                connectionSettingsView.getServerUrl(),
                connectionSettingsView.getUserName(),
                connectionSettingsView.getPassword());

        return connectionSettings;
    }

    private boolean isViewConnectionSettingsEmpty(){
        return StringUtils.isEmpty(connectionSettingsView.getServerUrl()) &&
                StringUtils.isEmpty(connectionSettingsView.getUserName()) &&
                StringUtils.isEmpty(connectionSettingsView.getPassword());
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
