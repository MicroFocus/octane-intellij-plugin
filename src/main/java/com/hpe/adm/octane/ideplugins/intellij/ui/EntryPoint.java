package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.components.WelcomeViewComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainPresenter;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.nonentity.SharedSpaceLevelRequestService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.annotations.NotNull;

/**
 * Entry point, will create the IntelliJ tool window
 */
public class EntryPoint implements ToolWindowFactory {

    /**
     * This method can be called by multiple IntelliJ's running at the same time,
     * it's important that the state of any one tool window that contains the IDE plugin
     * does not affect the state of other potentially open tool windows
     * @param project
     * @param toolWindow
     */
    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {

        // The DI module returns instances based on your current project
        // Be careful with all static members of beans, you might end up affecting the plugin open for another project
        // when dealing with 2 IntelliJ windows with two different projects running at the same time
        PluginModule pluginModule = new PluginModule(project);

        // Requited top level components, need to be injected by the above DI module,
        // all members of below classes will have support for field injection and constructor injection
        final ConnectionSettingsProvider connectionSettingsProvider = pluginModule.getInstance(ConnectionSettingsProvider.class);

        Runnable mainToolWindowContentControl = () -> {
            try{
                TestService testService = pluginModule.getInstance(TestService.class);

                //In case the connection is valid
                testService.testConnection(connectionSettingsProvider.getConnectionSettings());

                // Make sure you only instantiate other services (including the ones in the Presenter hierarchy,
                // after you tested the connection settings with the test service

                //Add the workspace name to the ToolWindow content tab name
                SharedSpaceLevelRequestService sharedSpaceLevelRequestService = pluginModule.getInstance(SharedSpaceLevelRequestService.class);
                String workspaceDisplayName = " [" + sharedSpaceLevelRequestService.getCurrentWorkspaceName() + "]";

                //Create the presenter hierarchy, DI will inject view instances
                MainPresenter mainPresenter = pluginModule.getInstance(MainPresenter.class);
                setContent(toolWindow, mainPresenter.getView(), workspaceDisplayName);

            } catch (Exception ex){
                //Otherwise show the welcome view
                setContent(toolWindow, new WelcomeViewComponent(project), "");
            }
        };

        //Run at the start of the application
        mainToolWindowContentControl.run();
        connectionSettingsProvider.addChangeHandler(mainToolWindowContentControl);
    }

    private void setContent(ToolWindow toolWindow, HasComponent hasComponent, String workspaceName) {
        ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
        toolWindow.getContentManager().removeAllContents(true);
        toolWindow.getContentManager().addContent(contentFactory.createContent(hasComponent.getComponent(), workspaceName, false));
    }

}
