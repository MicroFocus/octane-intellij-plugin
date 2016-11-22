package com.hpe.adm.octane.ideplugins.intellij;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.inject.*;
import com.google.inject.name.Named;
import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailView;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainView;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPaneView;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTableView;
import com.hpe.adm.octane.ideplugins.intellij.util.NotificationUtil;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
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
    }

    @Provides
    @Named("defaultTabbedPanePresenter")
    Presenter<TabbedPaneView> getTabbedPanePresenter(TabbedPaneView view){
        TabbedPanePresenter presenter = new TabbedPanePresenter();
        presenter.setView(view);
        return presenter;
    }

    @Provides
    @Named("defaultMainView")
    MainView getMainView(){
        MainView view = new MainView();
        return view;
    }

    @Provides
    @Named("defaultMainPresenter")
    MainPresenter getMainPresenter(MainView mainView,
                                   @Named("defaultTabbedPanePresenter") Presenter<TabbedPaneView> tabbedPanePresenter){
        MainPresenter presenter = new MainPresenter(tabbedPanePresenter);
        presenter.setView(mainView);
        return presenter;
    }

    @Provides
    @Named("defaultTabbedPaneView")
    TabbedPaneView getTabbedPaneView(){
        TabbedPaneView view = new TabbedPaneView();
        return view;
    }



    @Provides
    @Named("defaultEntityDetailView")
    EntityDetailView getEntityDetailView(){
        EntityDetailView view = new EntityDetailView();
        return view;
    }

    @Provides
    @Named("defaultEntityDetailPresenter")
    Presenter<EntityDetailView> getEntityDetailPresenter(EntityDetailView view){
        EntityDetailPresenter presenter = new EntityDetailPresenter();
        presenter.setView(view);
        return presenter;
    }

    @Provides
    @Named("defaultEntityTreeTableView")
    EntityTreeTableView getEntityTreeTableView(){
        EntityTreeTableView view = new EntityTreeTableView();
        return view;
    }

    @Provides
    @Named("defaultEntityTreeTablePresenter")
    Presenter<EntityTreeTableView> getEntityTreeTablePresenter(EntityTreeTableView view){
        EntityTreeTablePresenter presenter = new EntityTreeTablePresenter();
        presenter.setView(view);
        return presenter;
    }

}
