package com.hpe.adm.octane.services.nonentity;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.nga.sdk.network.HttpClient;
import com.hpe.adm.nga.sdk.network.HttpRequestFactory;
import com.hpe.adm.octane.services.connection.ConnectionSettings;
import com.hpe.adm.octane.services.connection.ConnectionSettingsProvider;

public class AuthenticationService{

    @Inject
    protected ConnectionSettingsProvider connectionSettingsProvider;

    protected HttpClient httpClient;

    protected HttpRequestFactory requestFactory;

    protected boolean authenticate() {
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();

        UserAuthorisation auth = new UserAuthorisation(connectionSettings.getUserName(), connectionSettings.getPassword());
        httpClient = HttpClient.getInstance();
        requestFactory = httpClient.getRequestFactory(connectionSettings.getBaseUrl(), auth);

        return httpClient.authenticate();
    }

}
