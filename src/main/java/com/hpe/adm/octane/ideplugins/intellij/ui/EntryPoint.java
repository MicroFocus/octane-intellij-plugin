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

package com.hpe.adm.octane.ideplugins.intellij.ui;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.components.WelcomeViewComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.LoadingWidget;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.services.MetadataService;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.nonentity.SharedSpaceLevelRequestService;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * Entry point, will create the IntelliJ tool window
 */
public class EntryPoint implements ToolWindowFactory {

    private static final Logger log = Logger.getInstance(EntryPoint.class);

    /**
     * This method can be called by multiple IntelliJ's running at the same time,
     * it's important that the state of any one tool window that contains the IDE plugin
     * does not affect the state of other potentially open tool windows
     *
     * @param project
     * @param toolWindow
     */
    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {

        // The DI module returns instances based on your current project
        // Be careful with all static members of beans, you might end up affecting the plugin open for another project
        // when dealing with 2 IntelliJ windows with two different projects running at the same time
        PluginModule pluginModule = PluginModule.getPluginModuleForProject(project);

        // Requited top level components, need to be injected by the above DI module,
        // all members of below classes will have support for field injection and constructor injection
        final ConnectionSettingsProvider connectionSettingsProvider = pluginModule.getInstance(ConnectionSettingsProvider.class);

        Runnable mainToolWindowContentControl = () -> {

            EntryPoint.this.setContent(toolWindow, () -> new LoadingWidget(), "");

            Task.Backgroundable backgroundTask = new Task.Backgroundable(project, "Loading Workspace", false) {
                public void run(@NotNull ProgressIndicator indicator) {
                    try {
                        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();

                        if (connectionSettings == null || connectionSettings.isEmpty()) {
                            throw new ServiceException("No connection settings configured");
                        }

                        TestService testService = pluginModule.getInstance(TestService.class);
                        testService.testConnection(connectionSettings);

                        // Make sure you only instantiate other services (including the ones in the Presenter hierarchy,
                        // after you tested the connection settings with the test service

                        //Add the workspace name to the ToolWindow content tab name
                        SharedSpaceLevelRequestService sharedSpaceLevelRequestService = pluginModule.getInstance(SharedSpaceLevelRequestService.class);
                        String workspaceDisplayName = " [" + sharedSpaceLevelRequestService.getCurrentWorkspaceName() + "]";

                        //Eager init the metadata service, it's an eager singleton
                        MetadataService metadataService = pluginModule.getInstance(MetadataService.class);
                        metadataService.eagerInit(Entity.values());

                        SwingUtilities.invokeAndWait(() -> {
                            //Create the presenter hierarchy, DI will inject view instances
                            MainPresenter mainPresenter = pluginModule.getInstance(MainPresenter.class);
                            setContent(toolWindow, mainPresenter.getView(), workspaceDisplayName);
                        });

                    } catch (Exception ex) {
                        pluginModule.getInstance(IdePluginPersistentState.class).clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
                        WelcomeViewComponent welcomeViewComponent;

                        // If there were previously configured connection settings
                        // show a slightly different message in the welcome view
                        if (!connectionSettingsProvider.getConnectionSettings().isEmpty()) {
                            welcomeViewComponent = new WelcomeViewComponent(
                                    project,
                                    "Your previously saved connection settings do not seem to work",
                                    "Please go to settings and test your connection to Octane",
                                    "Retry to connect Octane");

                            //also show a notification with the exception
                            UiUtil.showWarningBalloon(project,
                                    "Failed to connect to Octane",
                                    "Your previously saved connection settings do not seem to work <br> Error: " + ex.getMessage(),
                                    NotificationType.WARNING);
                        } else {
                            //In this case (probably), the plugin was never configured on this project before
                            welcomeViewComponent = new WelcomeViewComponent(project);
                        }

                        log.info("Showing welcome view, cause: " + ex);

                        //Show the welcome view
                        setContent(toolWindow, welcomeViewComponent, "");
                    }
                }
            };
            backgroundTask.queue();
        };

        //Run at the start of the application
        mainToolWindowContentControl.run();
        connectionSettingsProvider.addChangeHandler(mainToolWindowContentControl);
    }

    private void setContent(ToolWindow toolWindow, HasComponent hasComponent, String workspaceName) {
        SwingUtilities.invokeLater(() -> {
            ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
            toolWindow.getContentManager().removeAllContents(true);
            toolWindow.getContentManager().addContent(contentFactory.createContent(hasComponent.getComponent(), workspaceName, false));
        });
    }

}
