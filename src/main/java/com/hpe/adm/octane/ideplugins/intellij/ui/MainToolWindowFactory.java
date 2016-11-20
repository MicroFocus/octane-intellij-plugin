package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.google.inject.Guice;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.views.MainView;
import com.hpe.adm.octane.ideplugins.intellij.ui.views.WelcomeView;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import org.jetbrains.annotations.NotNull;

/**
 * Entry point, will create the IntelliJ tool window
 */
public class MainToolWindowFactory implements ToolWindowFactory {

    private ToolWindow toolWindow;

    @Inject
    private WelcomeView welcomeView;

    @Inject
    private MainView mainView;

    @Inject
    private TestService testService;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    private Injector injector = Guice.createInjector(new PluginModule());
    public MainToolWindowFactory(){
        injector.injectMembers(this);
    }

    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        this.toolWindow = toolWindow;

        Runnable mainToolWindowContentControl = () -> {
            try{
                testService.testConnection();
                //TODO: this view is not reloaded when the settings change
                //In case the connection is valid show a generalView at the start
                setContent(mainView.getContent());
            } catch (Exception ex){
                //Otherwise show the welcome view
                setContent(welcomeView.getContent());
            }
        };

        //Run at the start of the application
        mainToolWindowContentControl.run();

        //add handler
        connectionSettingsProvider.addChangeHandler(mainToolWindowContentControl);
    }

    /**
     * Swap the content of the main tool window
     * @param content
     */
    private void setContent(Content content){
        toolWindow.getContentManager().removeAllContents(true);
        toolWindow.getContentManager().addContent(content);
    }

}
