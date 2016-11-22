package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.google.inject.Guice;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.main.MainPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.annotations.NotNull;

/**
 * Entry point, will create the IntelliJ tool window
 */
public class MainToolWindowFactory implements ToolWindowFactory {

    private ToolWindow toolWindow;

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

        MainPresenter mainPresenter = new MainPresenter();
        TabbedPanePresenter tabbedPanePresenter = new TabbedPanePresenter();

        Runnable mainToolWindowContentControl = () -> {
            try{
                testService.testConnection();
                //TODO: this view is not reloaded when the settings change
                //In case the connection is valid show a generalView at the start
                setContent(tabbedPanePresenter.getView());

            } catch (Exception ex){
                //Otherwise show the welcome view
                setContent(mainPresenter.getView());
            }
        };

        //Run at the start of the application
        mainToolWindowContentControl.run();
        connectionSettingsProvider.addChangeHandler(mainToolWindowContentControl);
    }

    private void setContent(HasComponent hasComponent){
        ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
        toolWindow.getContentManager().removeAllContents(true);
        toolWindow.getContentManager().addContent(contentFactory.createContent(hasComponent.getComponent(), "", false));
    }

}
