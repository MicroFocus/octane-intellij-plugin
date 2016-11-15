package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.AbstractModule;
import com.google.inject.Singleton;

public class ServiceModule extends AbstractModule {
    @Override
    protected void configure() {
        bind(ConnectionSettings.class).toProvider(() -> new ConnectionSettings()).in(Singleton.class);
        bind(TestService.class).toProvider(() -> new TestService()).in(Singleton.class);
    }
}
