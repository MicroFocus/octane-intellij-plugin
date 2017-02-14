package com.hpe.adm.octane.ideplugins.intellij;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.common.eventbus.EventBus;
import com.google.inject.AbstractModule;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Provides;
import com.google.inject.name.Named;
import com.hpe.adm.nga.sdk.Octane;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.ToolbarActiveItem;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailView;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainView;
import com.hpe.adm.octane.ideplugins.intellij.ui.searchresult.SearchResultEntityTreeCellRenderer;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPaneView;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeCellRenderer;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeView;
import com.hpe.adm.octane.ideplugins.intellij.util.NotificationUtil;
import com.hpe.adm.octane.services.EntityService;
import com.hpe.adm.octane.services.MetadataService;
import com.hpe.adm.octane.services.TestService;
import com.hpe.adm.octane.services.UserService;
import com.hpe.adm.octane.services.connection.ConnectionSettings;
import com.hpe.adm.octane.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.services.connection.OctaneProvider;
import com.hpe.adm.octane.services.nonentity.SharedSpaceLevelRequestService;
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

    private PluginModule(Project project) {
        this.project = project;
        injectorSupplier = Suppliers.memoize(() -> Guice.createInjector(this));
        injectorMap.put(project, injectorSupplier);

        //TODO: this class needs a reason to exist
        getInstance(ToolbarActiveItem.class);
    }

    /**
     * Create an instance from an already existing PluginModule
     * @param project
     * @param injectorSupplier
     */
    private PluginModule(Project project, Supplier<Injector> injectorSupplier) {
        this.project = project;
        this.injectorSupplier = injectorSupplier;
    }

    public static boolean hasProject(Project project) {
        return injectorMap.containsKey(project);
    }

    public static PluginModule getPluginModuleForProject(Project project){
        if(hasProject(project)){
            return new PluginModule(project, injectorMap.get(project));
        }
        return new PluginModule(project);
    }

    public <T> T getInstance(Class<T> type) {
        return injectorSupplier.get().getInstance(type);
    }

    /**
     * CAREFUL: if there's a possibility that the module with the project does not exist yet,
     * this must be run on dispatch thread
     * @param project
     * @param type
     * @param <T>
     * @return
     */
    public static <T> T getInstance(Project project, Class<T> type) {
        if(!injectorMap.containsKey(project)){
            //Constructor changes static field event tho instance is not used anywhere
            new PluginModule(project);
        }
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
        bind(MetadataService.class).asEagerSingleton();
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
    @Named("searchEntityTreeView")
    public EntityTreeView getSearchEntityTreeView() {
        EntityTreeView entityTreeView = new EntityTreeView(new SearchResultEntityTreeCellRenderer());
        injectorSupplier.get().injectMembers(entityTreeView);
        return entityTreeView;
    }

    @Provides
    @Named("myWorkEntityTreeView")
    public EntityTreeView getMyWorkEntityTreeView() {
        EntityTreeView entityTreeView = new EntityTreeView(getInstance(EntityTreeCellRenderer.class));
        injectorSupplier.get().injectMembers(entityTreeView);
        return entityTreeView;
    }

    @Provides
    Project getProject(){
        return project;
    }

    //1 per plugin module
    private EventBus eventBus = new EventBus();

    @Provides
    EventBus getEventBus(){
        return eventBus;
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
