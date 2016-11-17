package com.hpe.adm.octane.ideplugins.intellij.ui.panels;

import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.wm.ToolWindow;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;

public class WelcomeView {

    private ToolWindow rootToolWindow;
    private JPanel rootPanel;

    private JLabel welcomeScreenMessage;
    private JXHyperlink settingsLink;

    public WelcomeView(){
        welcomeScreenMessage.setText("Welcome to ALM Octane plugin");
        settingsLink.setText("Before you start please go to settings and connect to Octane");
        settingsLink.addActionListener(event -> ShowSettingsUtil.getInstance().showSettingsDialog(null, ConnectionSettingsConfigurable.class));
    }

    public JPanel getRootPanel(){
        return rootPanel;
    }

}
