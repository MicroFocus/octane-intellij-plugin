package com.hpe.adm.octane.ideplugins.services.nonentity;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.nga.sdk.network.HttpClient;
import com.hpe.adm.nga.sdk.network.HttpRequestFactory;
import com.hpe.adm.octane.ideplugins.services.ServiceBase;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;

/**
 * Created by dulaut on 12/14/2016.
 */
public class AuthenticationService extends ServiceBase {

    @Inject
    protected ConnectionSettingsProvider connectionSettingsProvider;

    protected HttpClient httpClient;

    protected HttpRequestFactory requestFactory;

    protected boolean authenticate() {
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
//        ConnectionSettings connectionSettings = new ConnectionSettings("http://localhost:8080", 2001L, 1002L,
//                "sa@nga", "Welcome1");

        UserAuthorisation auth = new UserAuthorisation(connectionSettings.getUserName(), connectionSettings.getPassword());
        httpClient = HttpClient.getInstance();
        requestFactory = httpClient.getRequestFactory(connectionSettings.getBaseUrl(), auth);

        return httpClient.authenticate();
    }
}
