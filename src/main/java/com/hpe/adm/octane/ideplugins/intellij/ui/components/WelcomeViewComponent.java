package com.hpe.adm.octane.ideplugins.intellij.ui.components;

import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.intellij.openapi.options.ShowSettingsUtil;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;

/**
 * This does not need a presenter, it's too basic
 */
public class WelcomeViewComponent implements HasComponent {

    private JPanel rootPanel;

    private JLabel welcomeScreenMessage;
    private JXHyperlink settingsLink;

    public WelcomeViewComponent(){
        welcomeScreenMessage.setText("Welcome to ALM Octane plugin");
        settingsLink.setText("Before you start please go to settings and connect to Octane");
        settingsLink.addActionListener(event -> ShowSettingsUtil.getInstance().showSettingsDialog(null, ConnectionSettingsConfigurable.class));
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

}
