package com.hpe.adm.octane.ideplugins.intellij;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.inject.AbstractModule;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Singleton;
import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailView;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainView;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPaneView;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.intellij.util.NotificationUtil;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.nonentity.SharedSpaceLevelRequestService;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.diagnostic.Logger;

public class PluginModule extends AbstractModule {

    public static final Logger logger = Logger.getInstance("octane");

    protected static final Supplier<Injector> injectorSupplier = Suppliers.memoize(() -> Guice.createInjector(new PluginModule()));

    public PluginModule() {
    }

    public static <T> T getInstance(Class<T> type) {
        return injectorSupplier.get().getInstance(type);
    }

    public static Injector getInjectorSupplier(){
        return injectorSupplier.get();
    }

    @Override
    protected void configure() {
        bind(Logger.class).toInstance(logger);
        bind(Application.class).toInstance(ApplicationManager.getApplication());
        bind(NotificationUtil.class);

        //Settings
        bind(ConnectionSettingsConfigurable.class);
        bind(ConnectionSettingsProvider.class).toProvider(() -> ServiceManager.getService(IdePersistentConnectionSettingsProvider.class)).in(Singleton.class);

        //Services
        bind(TestService.class);
        bind(SharedSpaceLevelRequestService.class);

        //UI
        bind(MainView.class);
        bind(MainPresenter.class);

        bind(TabbedPaneView.class);
        bind(TabbedPanePresenter.class);

        bind(EntityDetailView.class);
        bind(EntityDetailPresenter.class);

        bind(EntityTreeTablePresenter.class);
    }

}
