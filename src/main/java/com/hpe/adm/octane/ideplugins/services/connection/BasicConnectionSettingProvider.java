package com.hpe.adm.octane.ideplugins.services.connection;

public class BasicConnectionSettingProvider implements ConnectionSettingsProvider{

    private final ConnectionSettings connectionSettings;

    public BasicConnectionSettingProvider(ConnectionSettings connectionSettings){
        this.connectionSettings = connectionSettings;
    }

    @Override
    public ConnectionSettings getConnectionSettings() {
        return connectionSettings;
    }

}
