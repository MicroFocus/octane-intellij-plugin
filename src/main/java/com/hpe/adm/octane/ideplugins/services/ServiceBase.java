package com.hpe.adm.octane.ideplugins.services;


import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.NGA;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;

public abstract class ServiceBase {

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    private static ConnectionSettings previousConnectionSettings = new ConnectionSettings();
    private static NGA nga;

    /**
     * Create an nga client with the current connection settings
     * Use this for all service methods that need an nga client
     * @return {@link NGA}
     */
    protected NGA getNGA(){
        ConnectionSettings currentConnectionSettings = connectionSettingsProvider.getConnectionSettings();

        if(!currentConnectionSettings.equals(previousConnectionSettings) || nga == null){
            nga = new NGA
                    .Builder(new UserAuthorisation(currentConnectionSettings.getUserName(), currentConnectionSettings.getPassword()))
                    .Server(currentConnectionSettings.getBaseUrl())
                    .sharedSpace(currentConnectionSettings.getSharedSpaceId())
                    .workSpace(currentConnectionSettings.getWorkspaceId())
                    .build();
        }

        previousConnectionSettings.setState(currentConnectionSettings);
        return nga;
    }

}
