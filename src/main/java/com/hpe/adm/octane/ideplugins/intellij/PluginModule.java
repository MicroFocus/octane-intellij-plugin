package com.hpe.adm.octane.ideplugins.intellij;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.inject.AbstractModule;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
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
import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.DataKeys;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.project.Project;

public class PluginModule extends AbstractModule {

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
        bind(Application.class).toInstance(ApplicationManager.getApplication());
        bind(NotificationUtil.class);

        bind(IdePluginPersistentState.class).toProvider(() -> ServiceManager.getService(getCurrentProject(), IdePluginPersistentState.class));
        bind(ConnectionSettingsProvider.class).toProvider(() -> ServiceManager.getService(getCurrentProject(), IdePersistentConnectionSettingsProvider.class));

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

    public static Project getCurrentProject(){
        DataContext dataContext = DataManager.getInstance().getDataContext();
        Project project = DataKeys.PROJECT.getData(dataContext);
        String name = project != null ? project.getName() : "N/A";
        System.out.println("Current Project: " + name);
        return project;
    }

}
