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

import com.hpe.adm.nga.sdk.authentication.Authentication;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.components.WelcomeViewComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.LoadingWidget;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.intellij.util.JavaFxUtils;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.connection.UserAuthentication;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.nonentity.SharedSpaceLevelRequestService;
import com.hpe.adm.octane.ideplugins.services.nonentity.TimelineService;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.ContentFactory;
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * Entry point, will create the IntelliJ tool window
 */
public class EntryPoint implements ToolWindowFactory {

    private static final Logger log = Logger.getInstance(EntryPoint.class);
    private Runnable mainToolWindowContentControl;

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

        if (!JavaFxUtils.isJavaFxAvailable()) {
            UiUtil.showWarningBalloon(project,
                    "JavaFx not found on current JVM",
                    "JVM running the IDEA platform does not have JavaFx installed, comments and memo fields might not be displayed properly.",
                    NotificationType.WARNING);
        }

        // The DI module returns instances based on your current project
        // Be careful with all static members of beans, you might end up affecting the plugin open for another project
        // when dealing with 2 IntelliJ windows with two different projects running at the same time
        PluginModule pluginModule = PluginModule.getPluginModuleForProject(project);

        // Requited top level components, need to be injected by the above DI module,
        // all members of below classes will have support for field injection and constructor injection
        final ConnectionSettingsProvider connectionSettingsProvider = pluginModule.getInstance(ConnectionSettingsProvider.class);

        mainToolWindowContentControl = () -> {

            EntryPoint.this.setContent(toolWindow, LoadingWidget::new, "");

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
                        final String toolWindowTabTitle = getToolWindowTabTitle(pluginModule);
                        SwingUtilities.invokeAndWait(() -> {
                            //Create the presenter hierarchy, DI will inject view instances
                            MainPresenter mainPresenter = pluginModule.getInstance(MainPresenter.class);
                            pluginModule.getInstance(EntityTreeTablePresenter.class).refresh();
                            setContent(toolWindow, mainPresenter.getView(), toolWindowTabTitle);
                        });
                    } catch (Exception ex) {
                        pluginModule.getInstance(IdePluginPersistentState.class).clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
                        WelcomeViewComponent welcomeViewComponent;

                        // If there were previously configured connection settings
                        // show a slightly different message in the welcome view
                        if (!connectionSettingsProvider.getConnectionSettings().isEmpty()) {

                            String message = ex.toString();
                            Authentication auth = connectionSettingsProvider.getConnectionSettings().getAuthentication();
                            if (auth instanceof UserAuthentication) {
                                UserAuthentication userAuthentication = (UserAuthentication) auth;
                                String username = userAuthentication.getUserName();
                                String password = userAuthentication.getPassword();
                                if (StringUtils.isNotEmpty(username) && StringUtils.isEmpty(password)) {
                                    message = "Please re-introduce your password.";
                                }
                            }

                            welcomeViewComponent = new WelcomeViewComponent(
                                    project,
                                    "Your previously saved connection settings do not seem to work",
                                    "Please go to settings and test your connection to Octane",
                                    "Retry to connect Octane");

                            //also show a notification with the exception
                            UiUtil.showWarningBalloon(project,
                                    "Failed to connect to Octane",
                                    "Your previously saved connection settings do not seem to work <br> Error: " + message,
                                    NotificationType.WARNING);

                        } else {
                            //In this case (probably), the plugin was never configured on this project before
                            welcomeViewComponent = new WelcomeViewComponent(project);
                        }

                        log.debug("Showing welcome view, cause: ", ex);

                        //Show the welcome view
                        setContent(toolWindow, welcomeViewComponent, "");

                    }
                }
            };
            backgroundTask.queue();
        };


        mainToolWindowContentControl.run();
        connectionSettingsProvider.addChangeHandler(mainToolWindowContentControl);
    }

    private String getToolWindowTabTitle(PluginModule pluginModule) {
        String workspaceName = getCurrentWorkspaceName(pluginModule);
        String shortTimeline = getShortTimeline(pluginModule);
        String toolWindowTitle = "";
        toolWindowTitle += workspaceName;
        toolWindowTitle += shortTimeline.isEmpty() ? "" : " | " + shortTimeline;
        return toolWindowTitle;
    }

    private String getCurrentWorkspaceName(PluginModule pluginModule) {
        String workspaceDisplayName;
        try {
            SharedSpaceLevelRequestService sharedSpaceLevelRequestService = pluginModule.getInstance(SharedSpaceLevelRequestService.class);
            workspaceDisplayName = "Ws: " + sharedSpaceLevelRequestService.getCurrentWorkspaceName();
        } catch (Exception ex) {
            workspaceDisplayName = "";
            log.warn("Failed to get workspace name: " + ex);
        }
        return workspaceDisplayName;
    }

    private String getShortTimeline(PluginModule pluginModule) {
        String timeline;
        try {
            TimelineService timelineService = pluginModule.getInstance(TimelineService.class);
            timeline = timelineService.getTimelineString();
        } catch (Exception ex) {
            timeline = "";
            log.warn("Failed to get timeline: " + ex);
        }
        return timeline;
    }

    private void setContent(ToolWindow toolWindow, HasComponent hasComponent, String workspaceName) {
        SwingUtilities.invokeLater(() -> {
            ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
            toolWindow.getContentManager().removeAllContents(true);
            toolWindow.getContentManager().addContent(contentFactory.createContent(hasComponent.getComponent(), workspaceName, false));
        });
    }


}
