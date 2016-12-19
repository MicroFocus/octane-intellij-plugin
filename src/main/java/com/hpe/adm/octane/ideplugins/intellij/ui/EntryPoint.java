package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.google.inject.Inject;
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

    @Inject
    SharedSpaceLevelRequestService sharedSpaceLevelRequestService;
    private ToolWindow toolWindow;
    @Inject
    private TestService testService;
    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        this.toolWindow = toolWindow;

        PluginModule.getInjectorSupplier().injectMembers(this);

        Runnable mainToolWindowContentControl = () -> {
            try{
                testService.testConnection(connectionSettingsProvider.getConnectionSettings());
                //In case the connection is valid show a generalView at the start
                MainPresenter mainPresenter = PluginModule.getInstance(MainPresenter.class);
                String workspaceDisplayName = " [" + sharedSpaceLevelRequestService.getCurrentWorkspaceName() + "]";
                setContent(mainPresenter.getView(), workspaceDisplayName);

            } catch (Exception ex){
                //Otherwise show the welcome view
                setContent(new WelcomeViewComponent(), "");
            }
        };

        //Run at the start of the application
        mainToolWindowContentControl.run();
        connectionSettingsProvider.addChangeHandler(mainToolWindowContentControl);
    }

    private void setContent(HasComponent hasComponent, String workspaceName) {
        ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
        toolWindow.getContentManager().removeAllContents(true);
        toolWindow.getContentManager().addContent(contentFactory.createContent(hasComponent.getComponent(), workspaceName, false));
    }

}
