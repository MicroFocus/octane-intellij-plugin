/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij.settings;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.components.ConnectionSettingsComponent;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceRuntimeException;
import com.hpe.adm.octane.ideplugins.services.nonentity.OctaneVersionService;
import com.hpe.adm.octane.ideplugins.services.util.OctaneVersion;
import com.hpe.adm.octane.ideplugins.services.util.UrlParser;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.MessageType;
import com.intellij.openapi.ui.popup.Balloon;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.ui.awt.RelativePoint;
import javafx.application.Platform;
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class ConnectionSettingsConfigurable implements SearchableConfigurable, Configurable.NoScroll {

    private static final String NAME = "Octane";
    private Project currentProject = null;

    //@Inject is not supported here, this class is instantiated by intellij
    private ConnectionSettingsProvider connectionSettingsProvider;
    private TestService testService;
    private IdePluginPersistentState idePluginPersistentState;
    private ConnectionSettingsComponent connectionSettingsView = new ConnectionSettingsComponent();
    private boolean pinMessage = false;

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

    public ConnectionSettingsConfigurable(@NotNull final Project currentProject) {
        PluginModule module = PluginModule.getPluginModuleForProject(currentProject);
        connectionSettingsProvider = module.getInstance(ConnectionSettingsProvider.class);
        testService = module.getInstance(TestService.class);
        idePluginPersistentState = module.getInstance(IdePluginPersistentState.class);
        this.currentProject = currentProject;
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
            new SwingWorker<Void, Void>() {
                @Override
                protected Void doInBackground() throws Exception {
                    testConnection();
                    return null;
                }
            }.execute();
        });

        return connectionSettingsView.getComponent();
    }

    @Override
    public boolean isModified() {
        //If it's empty and different allow apply
        if (isViewConnectionSettingsEmpty() && !connectionSettingsProvider.getConnectionSettings().isEmpty()) {
            return true;
        } else if (isViewConnectionSettingsEmpty()) {
            return false;
        }

        if (!pinMessage) {
            connectionSettingsView.setConnectionStatusLabelVisible(false);
        }

        ConnectionSettings currentConnectionSettings = connectionSettingsProvider.getConnectionSettings();
        ConnectionSettings viewConnectionSettings;
        try {
            viewConnectionSettings = validateClientSide();
        } catch (ServiceException ex) {
            pinMessage = false;
            return false;
        }

        return !viewConnectionSettings.equals(currentConnectionSettings);
    }

    @Override
    public void apply() throws ConfigurationException {
        //If the connection settings are empty then save them, only way to clear and save
        if (isViewConnectionSettingsEmpty()) {
            connectionSettingsProvider.setConnectionSettings(new ConnectionSettings());
            return;
        }

        ConnectionSettings newConnectionSettings = testConnection();

        //apply if valid
        if (newConnectionSettings != null) {

            //If anything other than the password was changed, wipe open tabs and active tab item
            if (!newConnectionSettings.equalsExceptPassword(connectionSettingsProvider.getConnectionSettings())) {
                idePluginPersistentState.clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
                idePluginPersistentState.clearState(IdePluginPersistentState.Key.SELECTED_TAB);
                idePluginPersistentState.clearState(IdePluginPersistentState.Key.OPEN_TABS);
            }

            connectionSettingsProvider.setConnectionSettings(newConnectionSettings);
            //remove the hash and remove extra stuff if successful
            connectionSettingsView.setServerUrl(UrlParser.createUrlFromConnectionSettings(newConnectionSettings));
            connectionSettingsView.setConnectionStatusSuccess();
        }
    }

    private void testOctaneVersion(ConnectionSettings connectionSettings) {
        OctaneVersion version;
        try {
            version = OctaneVersionService.getOctaneVersion(connectionSettings);
            version.discardBuildNumber();
            if (version.compareTo(OctaneVersion.DYNAMO) < 0) {
                showWarningBallon("Octane version not supported. This plugin works with Octane versions starting " + OctaneVersion.DYNAMO.getVersionString());
            }
        } catch (Exception ex) {
            version = OctaneVersionService.fallbackVersion;

            StringBuilder message = new StringBuilder();

            message.append("Failed to determine Octane server version, http call to ")
                    .append(OctaneVersionService.getServerVersionUrl(connectionSettings))
                    .append(" failed. Assuming server version is higher or equal to: ")
                    .append(version.getVersionString());

            showWarningBallon(message.toString());
        }
    }

    private void showWarningBallon(String message) {
        StatusBar statusBar = WindowManager.getInstance().getStatusBar(currentProject);
        Balloon balloon = JBPopupFactory.getInstance().createHtmlTextBalloonBuilder(message,
                MessageType.WARNING, null)
                .setCloseButtonEnabled(true)
                .createBalloon();
        balloon.show(RelativePoint.getCenterOf(statusBar.getComponent()), Balloon.Position.atRight);
    }

    /**
     * Test the connection with the given info from the view, sets error labels
     *
     * @return ConnectionSettings if valid, null otherwise
     */
    private ConnectionSettings testConnection() {

        connectionSettingsView.setConnectionStatusLoading();

        ConnectionSettings newConnectionSettings;
        try {
            newConnectionSettings = validateClientSide();
        } catch (ServiceException ex) {
            return null;
        }

        pinMessage = true;

        //This will attempt a connection
        try {
            testService.testConnection(newConnectionSettings);
            testOctaneVersion(newConnectionSettings);
            SwingUtilities.invokeLater(() -> { if(connectionSettingsView != null) connectionSettingsView.setConnectionStatusSuccess(); });
        } catch (ServiceException | ServiceRuntimeException ex) {
            //handle case when ok button is pressed
            SwingUtilities.invokeLater(() -> {
                if (connectionSettingsView != null) connectionSettingsView.setConnectionStatusError(ex.getMessage());
            });
            return null;
        }

        return newConnectionSettings;
    }

    private ConnectionSettings getConnectionSettingsFromView() throws ServiceException {
        //Parse server url
        return UrlParser.resolveConnectionSettings(
                connectionSettingsView.getServerUrl(),
                connectionSettingsView.getUserName(),
                connectionSettingsView.getPassword());
    }

    private boolean isViewConnectionSettingsEmpty() {
        return StringUtils.isEmpty(connectionSettingsView.getServerUrl()) &&
                StringUtils.isEmpty(connectionSettingsView.getUserName()) &&
                StringUtils.isEmpty(connectionSettingsView.getPassword());
    }

    private void validateUsernameAndPassword() throws ServiceException {
        StringBuilder errorMessageBuilder = new StringBuilder();
        if (StringUtils.isEmpty(connectionSettingsView.getUserName())) {
            errorMessageBuilder.append("Username cannot be blank.");
        }
        if (errorMessageBuilder.length() != 0) {
            errorMessageBuilder.append(" ");
        }
        if (StringUtils.isEmpty(connectionSettingsView.getPassword())) {
            errorMessageBuilder.append("Password cannot be blank.");
        }

        if (errorMessageBuilder.length() != 0) {
            throw new ServiceException(errorMessageBuilder.toString());
        }
    }

    private ConnectionSettings validateClientSide() throws ServiceException {
        ConnectionSettings newConnectionSettings;

        // Validation that does not require connection to the server,
        // only this one shows and example for a correct message
        try {
            newConnectionSettings = getConnectionSettingsFromView();
        } catch (ServiceException ex) {
            final StringBuilder errorMessageBuilder = new StringBuilder();
            errorMessageBuilder.append(ex.getMessage());
            errorMessageBuilder.append(Constants.CORRECT_URL_FORMAT_MESSAGE);
            SwingUtilities.invokeLater(() -> {
                if (connectionSettingsView != null) connectionSettingsView.setConnectionStatusError(ex.getMessage());
            });

            throw ex;

        }

        //Validation of username and password
        try {
            validateUsernameAndPassword();
        } catch (ServiceException ex) {
            //handle case when ok button is pressed
            SwingUtilities.invokeLater(() -> {
                if (connectionSettingsView != null) connectionSettingsView.setConnectionStatusError(ex.getMessage());
            });
            throw ex;
        }

        return newConnectionSettings;
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
