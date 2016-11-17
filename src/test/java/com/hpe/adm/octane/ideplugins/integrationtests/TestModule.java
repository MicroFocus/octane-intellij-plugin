package com.hpe.adm.octane.ideplugins.integrationtests;

import com.google.inject.AbstractModule;
import com.google.inject.Singleton;
import com.hpe.adm.octane.ideplugins.integrationtests.util.ConfigurationUtil;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.BasicConnectionSettingProvider;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;


/**
 * DI module for integration tests
 */
class TestModule extends AbstractModule {


    @Override
    protected void configure() {

        bind(TestService.class);

        bind(ConnectionSettingsProvider.class).toProvider(()-> {
            ConnectionSettings connectionSettings = new ConnectionSettings();

            //Set form config
            connectionSettings.setBaseUrl(ConfigurationUtil.getString(ConfigurationUtil.PropertyKeys.URL));
            connectionSettings.setSharedSpaceId(ConfigurationUtil.getLong(ConfigurationUtil.PropertyKeys.SHAREDSPACE));
            connectionSettings.setWorkspaceId(ConfigurationUtil.getLong(ConfigurationUtil.PropertyKeys.WORKSPACE));
            connectionSettings.setUserName(ConfigurationUtil.getString(ConfigurationUtil.PropertyKeys.USERNAME));
            connectionSettings.setPassword(ConfigurationUtil.getString(ConfigurationUtil.PropertyKeys.PASSWORD));

            return new BasicConnectionSettingProvider(connectionSettings);
        }).in(Singleton.class);

    }

}
