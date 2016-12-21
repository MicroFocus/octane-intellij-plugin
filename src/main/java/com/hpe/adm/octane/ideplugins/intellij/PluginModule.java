package com.hpe.adm.octane.ideplugins.intellij;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.inject.*;
import com.hpe.adm.nga.sdk.Octane;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailView;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainView;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPaneView;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeCellRenderer;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.intellij.util.NotificationUtil;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.UserService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.connection.OctaneProvider;
import com.hpe.adm.octane.ideplugins.services.nonentity.SharedSpaceLevelRequestService;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.project.Project;

import java.util.HashMap;
import java.util.Map;

public class PluginModule extends AbstractModule {

    protected final Supplier<Injector> injectorSupplier;

    private Project project;
    private static final Map<Project, Supplier<Injector>> injectorMap = new HashMap<>();

    public PluginModule(Project project) {
        this.project = project;
        injectorSupplier = Suppliers.memoize(() -> Guice.createInjector(this));
        injectorMap.put(project, injectorSupplier);
    }

    public <T> T getInstance(Class<T> type) {
        return injectorSupplier.get().getInstance(type);
    }

    public static <T> T getInstance(Project project, Class<T> type) {
        return injectorMap.get(project).get().getInstance(type);
    }

    @Override
    protected void configure() {

        bind(Application.class).toInstance(ApplicationManager.getApplication());
        bind(NotificationUtil.class);

        bind(IdePluginPersistentState.class).toProvider(() -> {
            IdePluginPersistentState idePluginPersistentState = ServiceManager.getService(project, IdePluginPersistentState.class);
            return idePluginPersistentState;
        });

        bind(ConnectionSettingsProvider.class).toProvider(() -> {
            ConnectionSettingsProvider connectionSettingsProvider = ServiceManager.getService(project, IdePersistentConnectionSettingsProvider.class);
            return connectionSettingsProvider;
        });

        //Services
        bind(TestService.class);
        bind(SharedSpaceLevelRequestService.class);
        bind(EntityService.class);
        bind(UserService.class);

        //UI
        bind(EntityTreeCellRenderer.class);

        bind(MainView.class);
        bind(MainPresenter.class);

        bind(TabbedPaneView.class);
        bind(TabbedPanePresenter.class);

        bind(EntityDetailView.class);
        bind(EntityDetailPresenter.class);

        bind(EntityTreeTablePresenter.class);

    }


    private ConnectionSettings previousConnectionSettings = new ConnectionSettings();
    private Octane octane;

    @Provides
    Project getProject(){
        return project;
    }

    /**
     * @return authenticated instance of Octane, with current connection settings
     */
    @Provides
    OctaneProvider getOctane(){
        return () -> {
            ConnectionSettings currentConnectionSettings = getInstance(ConnectionSettingsProvider.class).getConnectionSettings();
            if (!currentConnectionSettings.equals(previousConnectionSettings) || octane == null) {
                octane = new Octane
                        .Builder(new UserAuthorisation(currentConnectionSettings.getUserName(), currentConnectionSettings.getPassword()))
                        .Server(currentConnectionSettings.getBaseUrl())
                        .sharedSpace(currentConnectionSettings.getSharedSpaceId())
                        .workSpace(currentConnectionSettings.getWorkspaceId())
                        .build();

                previousConnectionSettings = currentConnectionSettings;
            }
            return octane;
        };
    }

}
