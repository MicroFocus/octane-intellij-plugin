package com.hpe.adm.octane.ideplugins.intellij.settings;

import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import org.jetbrains.annotations.Nullable;

@State(
        name = "ConnectionSettings",
        storages = {
                @Storage(
                        file = "$APP_CONFIG$/octane_connection_settings.xml"
                )}
)
public class IdePersistentConnectionSettingsProvider implements PersistentStateComponent<ConnectionSettings>, ConnectionSettingsProvider {

    private final ConnectionSettings connectionSettings = new ConnectionSettings();

    @Nullable
    @Override
    public ConnectionSettings getState() {
        return connectionSettings;
    }

    @Override
    public void loadState(ConnectionSettings state) {
        setConnectionSettings(state);
    }

    @Override
    public ConnectionSettings getConnectionSettings() {
        return connectionSettings;
    }

    @Override
    public void setConnectionSettings(ConnectionSettings connectionSettings) {
        this.connectionSettings.setBaseUrl(connectionSettings.getBaseUrl());
        this.connectionSettings.setPassword(connectionSettings.getPassword());
        this.connectionSettings.setUserName(connectionSettings.getUserName());
        this.connectionSettings.setSharedSpaceId(connectionSettings.getSharedSpaceId());
        this.connectionSettings.setWorkspaceId(connectionSettings.getWorkspaceId());
    }

}


