package com.hpe.adm.octane.ideplugins.services;


import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.Octane;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;

public abstract class ServiceBase {

    private static ConnectionSettings previousConnectionSettings = new ConnectionSettings();
    private static Octane octane;
    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    /**
     * Create an octane client with the current connection settings
     * Use this for all service methods that need an octane client
     * @return {@link Octane}
     */
    protected Octane getOctane() {

        ConnectionSettings currentConnectionSettings = connectionSettingsProvider.getConnectionSettings();

        if (!currentConnectionSettings.equals(previousConnectionSettings) || octane == null) {
            octane = new Octane
                    .Builder(new UserAuthorisation(currentConnectionSettings.getUserName(), currentConnectionSettings.getPassword()))
                    .Server(currentConnectionSettings.getBaseUrl())
                    .sharedSpace(currentConnectionSettings.getSharedSpaceId())
                    .workSpace(currentConnectionSettings.getWorkspaceId())
                    .build();
        }

        previousConnectionSettings = currentConnectionSettings;
        return octane;
    }

}
