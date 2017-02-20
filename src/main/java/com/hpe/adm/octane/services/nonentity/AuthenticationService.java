package com.hpe.adm.octane.services.nonentity;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.authentication.Authentication;
import com.hpe.adm.nga.sdk.authentication.SimpleUserAuthentication;
import com.hpe.adm.nga.sdk.network.google.GoogleHttpClient;
import com.hpe.adm.octane.services.connection.ConnectionSettings;
import com.hpe.adm.octane.services.connection.ConnectionSettingsProvider;

import static com.hpe.adm.octane.services.util.ClientType.HPE_MQM_UI;

public class AuthenticationService{

    @Inject
    protected ConnectionSettingsProvider connectionSettingsProvider;

    protected GoogleHttpClient httpClient;


    protected boolean authenticate() {
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();

        Authentication auth = new SimpleUserAuthentication(connectionSettings.getUserName(), connectionSettings.getPassword());
        httpClient = new GoogleHttpClient(connectionSettings.getBaseUrl(),HPE_MQM_UI.name());
        return httpClient.authenticate(auth);
    }

}
