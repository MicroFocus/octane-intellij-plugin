package com.hpe.adm.octane.ideplugins.intellij;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.inject.AbstractModule;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Singleton;
import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.intellij.ui.views.*;
import com.hpe.adm.octane.ideplugins.intellij.ui.views.treetable.EntityTreeTableView;
import com.hpe.adm.octane.ideplugins.intellij.util.NotificationUtil;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.diagnostic.Logger;

public class PluginModule extends AbstractModule {

    public static final Logger logger = Logger.getInstance("octane");

    protected static final Supplier<Injector> injector = Suppliers.memoize(() -> Guice.createInjector(new PluginModule()));

    public PluginModule() {
    }

    public static <T> T getInstance(Class<T> type) {
        return injector.get().getInstance(type);
    }

    @Override
    protected void configure() {
        bind(Logger.class).toInstance(logger);
        bind(Application.class).toInstance(ApplicationManager.getApplication());
        bind(NotificationUtil.class);

        //Settings
        bind(ConnectionSettingsConfigurable.class);
        bind(ConnectionSettingsProvider.class).toProvider(() -> ServiceManager.getService(IdePersistentConnectionSettingsProvider.class)).in(Singleton.class);

        //TODO: actually make the di useful
        //Views
        //Settings
        bind(ConnectionSettingsView.class);
        //Tool window content
        bind(WelcomeView.class);
        bind(MainView.class);
        /**/bind(FilteringView.class);
        /**/bind(TabView.class);
        /**//**/bind(EntityTreeTableView.class);

        //Services
        bind(TestService.class);
    }

}
