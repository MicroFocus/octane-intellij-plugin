package com.hpe.adm.octane.services.connection;

public interface ConnectionSettingsProvider {

    /**
     * This returns a copy, changing it won't change the provider
     * @return
     */
    ConnectionSettings getConnectionSettings();

    /**
     * This allows you to change what the provider returns, will fire the change handlers
     */
    void setConnectionSettings(ConnectionSettings connectionSettings);


    void addChangeHandler(Runnable observer);
}
