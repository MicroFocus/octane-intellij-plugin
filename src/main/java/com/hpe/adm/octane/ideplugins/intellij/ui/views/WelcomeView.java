package com.hpe.adm.octane.ideplugins.intellij.ui.views;

import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasContent;
import com.intellij.openapi.options.ShowSettingsUtil;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;

public class WelcomeView extends HasContent {

    private JPanel rootPanel;

    private JLabel welcomeScreenMessage;
    private JXHyperlink settingsLink;

    public WelcomeView(){
        welcomeScreenMessage.setText("Welcome to ALM Octane plugin");
        settingsLink.setText("Before you start please go to settings and connect to Octane");
        settingsLink.addActionListener(event -> ShowSettingsUtil.getInstance().showSettingsDialog(null, ConnectionSettingsConfigurable.class));
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

}
