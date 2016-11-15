package com.hpe.adm.octane.ideplugins;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.inject.*;
import com.hpe.adm.octane.ideplugins.intellij.ConnectionSettingsWrapper;
import com.hpe.adm.octane.ideplugins.intellij.ui.UiModule;
import com.hpe.adm.octane.ideplugins.services.ServiceModule;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileEditor.FileDocumentManager;

public class PluginModule extends AbstractModule {
    public static final Logger LOG = Logger.getInstance("octane");
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
        bind(Logger.class).toInstance(LOG);
        bind(Application.class).toInstance(ApplicationManager.getApplication());
        bind(FileDocumentManager.class).toInstance(FileDocumentManager.getInstance());

        install(new UiModule());


        Provider<ConnectionSettingsWrapper> settingsProvider = () -> ServiceManager.getService(ConnectionSettingsWrapper.class);
        bind(ConnectionSettingsWrapper.class).toProvider(settingsProvider).in(Singleton.class);

        //A service module only works on one set of connection settings
        install(new ServiceModule(settingsProvider.get().getConnectionSettings()));

    }

}
