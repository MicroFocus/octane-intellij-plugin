package com.hpe.adm.octane.ideplugins.integrationtests;

import com.google.inject.Guice;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.hpe.adm.nga.sdk.Octane;
import com.hpe.adm.nga.sdk.authentication.SimpleUserAuthentication;
import com.hpe.adm.octane.services.connection.ConnectionSettings;
import com.hpe.adm.octane.services.connection.ConnectionSettingsProvider;
import org.junit.Before;

/**
 * Enables the use of the {@link com.google.inject.Inject} annotation
 */
public abstract class IntegrationTestBase {

    private Injector injector = Guice.createInjector(new TestModule());

    @Inject
    ConnectionSettingsProvider connectionSettingsProvider;

    @Before
    public void setup () {
        injector.injectMembers(this);
    }

    public Octane getOctane(){
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
        return new Octane
                .Builder(new SimpleUserAuthentication(connectionSettings.getUserName(), connectionSettings.getPassword()))
                .Server(connectionSettings.getBaseUrl())
                .sharedSpace(connectionSettings.getSharedSpaceId())
                .workSpace(connectionSettings.getWorkspaceId())
                .build();
    }

}
