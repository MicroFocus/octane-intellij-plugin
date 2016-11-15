package com.hpe.adm.octane.ideplugins.integrationtests;

import com.google.inject.AbstractModule;
import com.google.inject.Singleton;
import com.hpe.adm.octane.ideplugins.services.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.TestService;

public class TestModule extends AbstractModule {

    @Override
    protected void configure() {
        bind(TestService.class);
        bind(ConnectionSettings.class).toProvider(()-> new ConnectionSettings()).in(Singleton.class);
    }

}
