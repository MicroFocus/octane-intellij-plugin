package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.AbstractModule;
import com.google.inject.Singleton;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;

//TODO: Currently not using the modules, having problems sharing a bean between more modules
public class ServiceModule extends AbstractModule {

    @Override
    protected void configure() {
        bind(TestService.class).toProvider(TestService::new).in(Singleton.class);
    }
}
