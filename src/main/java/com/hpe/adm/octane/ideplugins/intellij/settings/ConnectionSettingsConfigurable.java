/*
 * © Copyright 2017-2022 Micro Focus or one of its affiliates.
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

import com.google.api.client.http.HttpResponseException;
import com.hpe.adm.nga.sdk.authentication.Authentication;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.components.ConnectionSettingsComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.searchresult.SearchHistoryManager;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.intellij.util.ExceptionHandler;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.connection.UserAuthentication;
import com.hpe.adm.octane.ideplugins.services.connection.granttoken.GrantTokenAuthentication;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.nonentity.OctaneVersionService;
import com.hpe.adm.octane.ideplugins.services.util.OctaneVersion;
import com.hpe.adm.octane.ideplugins.services.util.UrlParser;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.MessageType;
import com.intellij.openapi.ui.popup.Balloon;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.ui.awt.RelativePoint;
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Objects;

public class ConnectionSettingsConfigurable implements SearchableConfigurable, Configurable.NoScroll {

    private static final String NAME = "Octane";
    private Project currentProject;

    //@Inject is not supported here, this class is instantiated by intellij
    private ConnectionSettingsProvider connectionSettingsProvider;
    private TestService testService;
    private IdePluginPersistentState idePluginPersistentState;
    private ConnectionSettingsComponent connectionSettingsView;
    private SearchHistoryManager searchManager;
    private boolean pinMessage = false;
    private final PluginModule pluginModule;

    public ConnectionSettingsConfigurable(@NotNull final Project currentProject) {
        pluginModule = PluginModule.getPluginModuleForProject(currentProject);
        connectionSettingsView = new ConnectionSettingsComponent(currentProject);
        connectionSettingsProvider = pluginModule.getInstance(ConnectionSettingsProvider.class);
        testService = pluginModule.getInstance(TestService.class);
        idePluginPersistentState = pluginModule.getInstance(IdePluginPersistentState.class);
        searchManager = pluginModule.getInstance(SearchHistoryManager.class);
        this.currentProject = currentProject;
    }

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

        if (connectionSettings.getAuthentication() instanceof UserAuthentication) {
            connectionSettingsView.setSsoAuth(false);
            UserAuthentication authentication = (UserAuthentication) connectionSettings.getAuthentication();
            connectionSettingsView.setUserName(authentication.getAuthenticationId());
            connectionSettingsView.setPassword(authentication.getAuthenticationSecret());
        } else if (connectionSettings.getAuthentication() instanceof GrantTokenAuthentication) {
            connectionSettingsView.setSsoAuth(true);
        }

        connectionSettingsView.setTestConnectionActionListener(event -> new SwingWorker<Void, Void>() {
            @Override
            protected Void doInBackground() {
                connectionSettingsView.setConnectionStatusLabelVisible(false);
                testConnection();
                return null;
            }
        }.execute());

        connectionSettingsView.addResetUserActionListener(e -> {
            if (connectionSettingsView.isSsoAuth()) {
                apply();
            }
        });

        return connectionSettingsView.getComponent();
    }

    @Override
    public boolean isModified() {

        connectionSettingsView.refreshProxySettingsLabel();

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

        // compare auth method
        if (!(currentConnectionSettings.getAuthentication() instanceof GrantTokenAuthentication) && connectionSettingsView.isSsoAuth()) {
            return true;
        }

        // compare auth data
        if (connectionSettingsView.isSsoAuth()) {
            return !viewConnectionSettings.equalsExceptAuth(currentConnectionSettings);
        } else {
            return !viewConnectionSettings.equals(currentConnectionSettings);
        }
    }

    @Override
    public void apply() {
        //If the connection settings are empty then save them, only way to clear and save
        if (isViewConnectionSettingsEmpty()) {
            connectionSettingsProvider.setConnectionSettings(new ConnectionSettings());
            return;
        }
        ConnectionSettings newConnectionSettings = null;
        try {
            newConnectionSettings = testConnection();
            //We should not have search history from any previous workspaces
            searchManager.clearSearchHistory();
        } catch (Exception ex) {
            ExceptionHandler exceptionHandler = new ExceptionHandler(ex, currentProject);
            exceptionHandler.showErrorNotification();
        }

        //apply if valid
        if (newConnectionSettings != null) {

            //If anything other than the password was changed, wipe open tabs and active tab item
            boolean isServerInfoChanged = !newConnectionSettings.equalsExceptAuth(connectionSettingsProvider.getConnectionSettings());

            Class<? extends Authentication> authType;
            if(connectionSettingsProvider.getConnectionSettings().getAuthentication() != null) {
                authType = connectionSettingsProvider.getConnectionSettings().getAuthentication().getClass();
            } else {
                authType = null;
            }

            Class<? extends Authentication> newAuthType = newConnectionSettings.getAuthentication().getClass();
            boolean isAuthTypeChanged = !Objects.equals(authType, newAuthType);

            if (isServerInfoChanged || isAuthTypeChanged) {
                idePluginPersistentState.clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
                idePluginPersistentState.clearState(IdePluginPersistentState.Key.PREV_ACTIVE_WORK_ITEM);
                idePluginPersistentState.clearState(IdePluginPersistentState.Key.SELECTED_TAB);
                idePluginPersistentState.clearState(IdePluginPersistentState.Key.OPEN_TABS);
            }
            connectionSettingsProvider.setConnectionSettings(newConnectionSettings);
            //remove the hash and remove extra stuff if successful
            connectionSettingsView.setServerUrl(UrlParser.createUrlFromConnectionSettings(newConnectionSettings));
            connectionSettingsView.setConnectionStatusSuccess();

            SwingUtilities.invokeLater(() -> {
                try {
                    pluginModule.getInstance(TabbedPanePresenter.class).closeAllTabsExceptMyWork();
                } catch (Exception ignored) {
                }
            });
        }
    }

    private void testOctaneVersion(ConnectionSettings connectionSettings) {
        OctaneVersion version;
        try {
            version = OctaneVersionService.getOctaneVersion(connectionSettings);
            version.discardBuildNumber();
            if (version.compareTo(OctaneVersion.DYNAMO) < 0) {
                showWarningBalloon("Octane version not supported. This plugin works with Octane versions starting " + OctaneVersion.DYNAMO.getVersionString());
            }

            if (connectionSettingsView.isSsoAuth()) {
                if (version.compareTo(new OctaneVersion("12.60.14")) < 0) {
                    showWarningBalloon("Login with browser is only supported starting from Octane server version: " + OctaneVersion.INTER_P2.getVersionString());
                    connectionSettingsView.setSsoAuth(false);
                }
            }

        } catch (Exception ex) {
            version = OctaneVersionService.fallbackVersion;

            String message = "Failed to determine Octane server version, http call to " +
                    OctaneVersionService.getServerVersionUrl(connectionSettings) +
                    " failed. Assuming server version is higher or equal to: " +
                    version.getVersionString();

            showWarningBalloon(message);
        }
    }

    private void showWarningBalloon(String message) {
        Balloon balloon = JBPopupFactory.getInstance().createHtmlTextBalloonBuilder(message,
                MessageType.WARNING, null)
                .setCloseButtonEnabled(true)
                .createBalloon();

        balloon.show(RelativePoint.getCenterOf(connectionSettingsView.getComponent()), Balloon.Position.atRight);
    }

    /**
     * Test the connection with the given info from the view, sets error labels
     *
     * @return ConnectionSettings if valid, null otherwise
     */
    private ConnectionSettings testConnection() {

        ConnectionSettings newConnectionSettings;
        try {
            newConnectionSettings = validateClientSide();
        } catch (ServiceException ex) {
            return null;
        }

        pinMessage = true;

        try {
            connectionSettingsView.setConnectionStatusLoading();

            //This will attempt a connection
            testService.testConnection(newConnectionSettings);

            testOctaneVersion(newConnectionSettings);

            SwingUtilities.invokeLater(() -> {
                if (connectionSettingsView != null) connectionSettingsView.setConnectionStatusSuccess();
            });

        } catch (Exception ex) {

            SwingUtilities.invokeLater(() -> {
                if (connectionSettingsView != null) {
                    if (ex instanceof OctaneException) {
                        OctaneException octaneException = (OctaneException) ex;
                        StringFieldModel errorDescription = (StringFieldModel) octaneException.getError().getValue("description");
                        if (errorDescription != null) {
                            connectionSettingsView.setConnectionStatusError(errorDescription.getValue());
                        }
                    } else if (ex instanceof RuntimeException && ex.getCause() != null && ex.getCause() instanceof HttpResponseException) {
                        HttpResponseException responseException = (HttpResponseException) ex.getCause();
                        if (responseException.getStatusCode() == 401) {
                            connectionSettingsView.setConnectionStatusError("Invalid username or password.");
                        }
                    } else {
                        connectionSettingsView.setConnectionStatusError("Failed to connect to octane: " + ex.getMessage());
                    }
                }
            });

            return null;
        }

        return newConnectionSettings;
    }

    private ConnectionSettings getConnectionSettingsFromView() throws ServiceException {

        //Parse server url
        ConnectionSettings connectionSettings =
                UrlParser.resolveConnectionSettings(
                        connectionSettingsView.getServerUrl(),
                        connectionSettingsView.getUserName(),
                        connectionSettingsView.getPassword());

        if (connectionSettingsView.isSsoAuth()) {
            connectionSettings.setAuthentication(new GrantTokenAuthentication());
        }

        return connectionSettings;
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
                if (connectionSettingsView != null) {
                    connectionSettingsView.setConnectionStatusError(errorMessageBuilder.toString());
                }
            });

            throw ex;

        }

        //Validation of username and password
        if (!connectionSettingsView.isSsoAuth()) {
            try {
                validateUsernameAndPassword();
            } catch (ServiceException ex) {
                //handle case when ok button is pressed
                SwingUtilities.invokeLater(() -> {
                    if (connectionSettingsView != null) {
                        connectionSettingsView.setConnectionStatusError(ex.getMessage());
                    }
                });
                throw ex;
            }
        }

        return newConnectionSettings;
    }

    @Override
    public void reset() {
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
        connectionSettingsView.setServerUrl(UrlParser.createUrlFromConnectionSettings(connectionSettings));

        if (connectionSettings.getAuthentication() instanceof UserAuthentication) {
            UserAuthentication authentication = (UserAuthentication) connectionSettings.getAuthentication();
            connectionSettingsView.setUserName(authentication.getAuthenticationId());
            connectionSettingsView.setPassword(authentication.getAuthenticationSecret());
        }
    }

    @Override
    public void disposeUIResources() {
        connectionSettingsView = null;
    }

}
