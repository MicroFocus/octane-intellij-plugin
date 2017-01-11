package com.hpe.adm.octane.ideplugins.intellij.ui.components;

import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;

/**
 * This does not need a presenter, it's too basic
 */
public class WelcomeViewComponent implements HasComponent {

    private static final String OCTANE_SETTINGS_TEXT = "To start, go to Settings in the ribbon above and connect to ALM Octane";
    private static final String WELCOME_TEXT = "Welcome to ALM Octane plugin";
    private JPanel rootPanel;

    private JLabel welcomeScreenMessage;
    private JXHyperlink settingsLink;

    public WelcomeViewComponent(Project project){
        welcomeScreenMessage.setText(WELCOME_TEXT);
        settingsLink.setText(OCTANE_SETTINGS_TEXT);
        settingsLink.addActionListener(event -> ShowSettingsUtil.getInstance().showSettingsDialog(project, ConnectionSettingsConfigurable.class));
    }

    public WelcomeViewComponent(Project project, String welcomeMessage, String settingsLinkMessage){
        welcomeScreenMessage.setText(welcomeMessage);
        settingsLink.setText(settingsLinkMessage);
        settingsLink.addActionListener(event -> ShowSettingsUtil.getInstance().showSettingsDialog(project, ConnectionSettingsConfigurable.class));
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

}
