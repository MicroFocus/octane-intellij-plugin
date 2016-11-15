package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.intellij.ui.panels.ConnectionSettingsView;
import com.hpe.adm.octane.ideplugins.intellij.util.NotificationUtil;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.intellij.openapi.components.ServiceManager;

//TODO: Currently not using the modules, having problems sharing a bean between more modules
public class UiModule extends AbstractModule {
    @Override
    protected void configure() {
        bind(ConnectionSettingsView.class);
        bind(ConnectionSettingsConfigurable.class);
        bind(NotificationUtil.class);

        bind(IdePersistentConnectionSettingsProvider.class).toProvider(() -> ServiceManager.getService(IdePersistentConnectionSettingsProvider.class)).in(Singleton.class);
    }



}