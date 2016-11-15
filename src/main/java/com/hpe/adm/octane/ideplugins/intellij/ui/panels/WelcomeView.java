package com.hpe.adm.octane.ideplugins.intellij.ui.panels;

import com.hpe.adm.octane.ideplugins.intellij.ConnectionSettingsConfigurable;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import org.jdesktop.swingx.JXHyperlink;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class WelcomeView implements ToolWindowFactory{

    private Project project;

    private ToolWindow rootToolWindow;
    private JPanel rootPanel;

    private JLabel welcomeScreenMessage;
    private JXHyperlink settingsLink;

    private void createUIComponents() {
        welcomeScreenMessage.setText("Welcome to ALM Octane plugin");
        settingsLink.setText("Before you start please go to settings and connect to Octane");
    }

    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow rootToolWindow) {
        this.rootToolWindow = rootToolWindow;
        this.project = project;

        this.createUIComponents();

        ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
        Content content = contentFactory.createContent(rootPanel, "", false);
        rootToolWindow.getContentManager().addContent(content);

        settingsLink.addActionListener(event -> {
            ShowSettingsUtil.getInstance().showSettingsDialog(project, ConnectionSettingsConfigurable.class);
        });
    }
}
