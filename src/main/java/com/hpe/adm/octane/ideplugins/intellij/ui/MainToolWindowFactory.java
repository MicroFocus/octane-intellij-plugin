package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.google.inject.Guice;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.MainView;
import com.hpe.adm.octane.ideplugins.intellij.ui.WelcomeView;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

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
        ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();

        JPanel mainPanel = new JPanel();
        mainPanel.add(new JLabel("It's working, be happy!"), BorderLayout.CENTER);

        Runnable mainToolWindowContentControl = () -> {
            try{
                testService.testConnection();
                //In case the connection is valid show a generalView at the start
                setContent(mainView.getContent());
            } catch (Exception ex){
                //Otherwise show the welcome view
                setContent(welcomeView.getContent());
            }
        };

        mainToolWindowContentControl.run();

        //add handler
        connectionSettingsProvider.addChangeHandler(mainToolWindowContentControl);
    }

    private void setContent(Content content){
        toolWindow.getContentManager().removeAllContents(true);
        toolWindow.getContentManager().addContent(content);
    }

}
