package com.hpe.adm.octane.ideplugins.intellij;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.inject.*;
import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.intellij.ui.panels.ConnectionSettingsView;
import com.hpe.adm.octane.ideplugins.intellij.util.NotificationUtil;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileEditor.FileDocumentManager;

public class PluginModule extends AbstractModule {

    public static final Logger logger = Logger.getInstance("octane");

    protected static final Supplier<Injector> injector = Suppliers.memoize(new Supplier<Injector>() {
        @Override
        public Injector get() {
            return Guice.createInjector(new PluginModule());
        }
    });

    public PluginModule() {
    }

    public static <T> T getInstance(Class<T> type) {
        return injector.get().getInstance(type);
    }

    @Override
    protected void configure() {

        bind(Logger.class).toInstance(logger);
        bind(Application.class).toInstance(ApplicationManager.getApplication());
        bind(FileDocumentManager.class).toInstance(FileDocumentManager.getInstance());

        bind(ConnectionSettingsView.class);
        bind(ConnectionSettingsConfigurable.class);
        bind(NotificationUtil.class);
        bind(IdePersistentConnectionSettingsProvider.class).toProvider(() -> ServiceManager.getService(IdePersistentConnectionSettingsProvider.class)).in(Singleton.class);

        //Services
        bind(TestService.class);

    }

    //Should provide the connection settings for the services
    @Provides
    ConnectionSettings getConnectionSettings(IdePersistentConnectionSettingsProvider connectionSettingsProvider) {
        return connectionSettingsProvider.getConnectionSettings();
    }
}
