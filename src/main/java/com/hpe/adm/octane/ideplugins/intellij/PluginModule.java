/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.common.eventbus.EventBus;
import com.google.inject.AbstractModule;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Provides;
import com.google.inject.name.Named;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.settings.LoginDialog;
import com.hpe.adm.octane.ideplugins.intellij.ui.ToolbarActiveItem;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.searchresult.SearchResultEntityTreeCellRenderer;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeCellRenderer;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeView;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.connection.granttoken.TokenPollingCompleteHandler;
import com.hpe.adm.octane.ideplugins.services.connection.granttoken.TokenPollingInProgressHandler;
import com.hpe.adm.octane.ideplugins.services.connection.granttoken.TokenPollingStartedHandler;
import com.hpe.adm.octane.ideplugins.services.di.ServiceModule;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;

import javax.swing.*;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

public class PluginModule extends AbstractModule {

    private static final Logger logger = Logger.getInstance(PluginModule.class.getName());

    protected final Supplier<Injector> injectorSupplier;

    private Project project;
    private LoginDialog loginDialog;
    private static final Map<Project, Supplier<Injector>> injectorMap = new HashMap<>();

    private PluginModule(Project project) {

        this.project = project;

        ConnectionSettingsProvider connectionSettingsProvider = ServiceManager.getService(project, IdePersistentConnectionSettingsProvider.class);

        TokenPollingStartedHandler pollingStartedHandler = loginPageUrl -> SwingUtilities.invokeLater(() -> {
            loginDialog = new LoginDialog(project, loginPageUrl);
            loginDialog.show();
        });

        TokenPollingInProgressHandler pollingInProgressHandler = pollingStatus -> {
            try {
                SwingUtilities.invokeAndWait(() -> {
                    if(loginDialog != null) {
                        long secondsUntilTimeout = (pollingStatus.timeoutTimeStamp - System.currentTimeMillis()) / 1000;
                        loginDialog.setTitle(LoginDialog.TITLE + " (waiting for session, timeout in: " + secondsUntilTimeout + ")");
                        pollingStatus.shouldPoll = loginDialog.getExitCode() == DialogWrapper.CANCEL_EXIT_CODE;
                    }
                });
            } catch (InterruptedException | InvocationTargetException e) {
                logger.error(e);
            }
            return pollingStatus;
        };

        TokenPollingCompleteHandler pollingCompleteHandler = tokenPollingCompletedStatus -> {
            try {
                SwingUtilities.invokeAndWait(() -> loginDialog.close(0, true));
            } catch (InterruptedException | InvocationTargetException e) {
                e.printStackTrace();
            }
        };

        ServiceModule serviceModule = new ServiceModule(connectionSettingsProvider, pollingStartedHandler, pollingInProgressHandler, pollingCompleteHandler);

        injectorSupplier = Suppliers.memoize(() -> Guice.createInjector(serviceModule, this));
        injectorMap.put(project, injectorSupplier);
        getInstance(ToolbarActiveItem.class);
    }

    /**
     * Create an instance from an already existing PluginModule
     *
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

    public static PluginModule getPluginModuleForProject(Project project) {
        if (hasProject(project)) {
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
     *
     * @param project
     * @param type
     * @param <T>
     * @return
     */
    public static <T> T getInstance(Project project, Class<T> type) {
        if (!injectorMap.containsKey(project)) {
            //Constructor changes static field event tho instance is not used anywhere
            new PluginModule(project);
        }
        return injectorMap.get(project).get().getInstance(type);
    }

    @Override
    protected void configure() {
        bind(IdePluginPersistentState.class)
                .toProvider(() -> ServiceManager.getService(project, IdePluginPersistentState.class));
    }

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
    Project getProject() {
        return project;
    }

    //1 per plugin module
    private EventBus eventBus = new EventBus();

    @Provides
    EventBus getEventBus() {
        return eventBus;
    }

}
