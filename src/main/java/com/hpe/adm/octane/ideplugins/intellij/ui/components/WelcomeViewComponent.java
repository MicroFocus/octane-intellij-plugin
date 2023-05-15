/*
 * © Copyright 2017-2022 Micro Focus or one of its affiliates.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij.ui.components;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork.NoWorkPanel;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import com.intellij.util.ui.UIUtil;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;

public class WelcomeViewComponent extends JPanel implements HasComponent {

    private static final long serialVersionUID = 1L;
    private static final String WELCOME_TEXT = "Welcome to ALM Octane plugin";
    private static final String OCTANE_SETTINGS_TEXT = "To start, go to Settings and connect.";
    private static final String OCTANE_SETTINGS_RETRY = "Retry";
    private JXHyperlink hyperlinkSettings;
    private JXHyperlink hyperlinkRetry;
    private JLabel lblMessage;
    private boolean connectionFailed;

    public WelcomeViewComponent() {
        init();
    }

    public WelcomeViewComponent(Project project) {
        init();
        hyperlinkSettings.addActionListener(event -> ShowSettingsUtil.getInstance().showSettingsDialog(project, ConnectionSettingsConfigurable.class));
    }

    public WelcomeViewComponent(Project project, String welcomeMessage, String settingsLinkMessage, String retryMessage) {
        final ConnectionSettingsProvider connectionSettingsProvider = PluginModule.getPluginModuleForProject(project).getInstance(ConnectionSettingsProvider.class);
        connectionFailed = retryMessage != null;
        init();
        lblMessage.setText(welcomeMessage);
        hyperlinkSettings.setText(settingsLinkMessage);
        hyperlinkSettings.addActionListener(event -> ShowSettingsUtil.getInstance().showSettingsDialog(project, ConnectionSettingsConfigurable.class));
        if (connectionFailed) {
            hyperlinkRetry.setText(retryMessage);
            hyperlinkRetry.addActionListener(event -> {
                        connectionSettingsProvider.setConnectionSettings(connectionSettingsProvider.getConnectionSettings());
                    }
            );
        }
    }


    /**
     * Create the panel.
     */
    private void init() {
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0, 0, 0, 0, 0};
        gridBagLayout.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{0.0, 1.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
        setLayout(gridBagLayout);

        JLabel lblCompany = new JLabel("");
        lblCompany.setIcon(new ImageIcon(NoWorkPanel.class.getResource(Constants.IMG_VENDOR_LOGO_LIGHT)));
        GridBagConstraints gbc_lblCompany = new GridBagConstraints();
        gbc_lblCompany.anchor = GridBagConstraints.SOUTHEAST;
        gbc_lblCompany.fill = GridBagConstraints.VERTICAL;
        gbc_lblCompany.insets = new Insets(0, 0, 10, 10);
        gbc_lblCompany.gridx = 1;
        gbc_lblCompany.gridy = 5;
        add(lblCompany, gbc_lblCompany);


        JLabel lblOctane = new JLabel("");
        lblOctane.setIcon(new ImageIcon(NoWorkPanel.class.getResource(Constants.IMG_OCTANE_LOGO)));
        GridBagConstraints gbc_lblOctane = new GridBagConstraints();
        gbc_lblOctane.anchor = GridBagConstraints.CENTER;
        gbc_lblOctane.fill = GridBagConstraints.VERTICAL;
        gbc_lblOctane.insets = new Insets(10, 0, 0, 0);
        gbc_lblOctane.gridx = 1;
        gbc_lblOctane.gridy = 1;
        add(lblOctane, gbc_lblOctane);

        lblMessage = new JLabel(WELCOME_TEXT);
        Font lblMessageFont = new Font(lblMessage.getFont().getFontName(),Font.PLAIN,18);
        lblMessage.setFont(lblMessageFont);
        GridBagConstraints gbc_lblMessage = new GridBagConstraints();
        gbc_lblMessage.anchor = GridBagConstraints.CENTER;
        gbc_lblMessage.insets = new Insets(0, 0, 5, 0);
        gbc_lblMessage.gridx =1;
        gbc_lblMessage.gridy = 2;
        add(lblMessage, gbc_lblMessage);

        hyperlinkSettings = new JXHyperlink();
        hyperlinkSettings.setText(OCTANE_SETTINGS_TEXT);
        Font  hyperlinkSettingsFont = new Font(hyperlinkSettings.getFont().getFontName(),Font.PLAIN,18);
        hyperlinkSettings.setFont(hyperlinkSettingsFont);
        GridBagConstraints gbc_hyperlinkSettings = new GridBagConstraints();
        gbc_hyperlinkSettings.anchor = GridBagConstraints.CENTER;
        gbc_hyperlinkSettings.insets = new Insets(0, 0, 5, 0);
        gbc_hyperlinkSettings.gridx = 1;
        gbc_hyperlinkSettings.gridy = 3;
        add(hyperlinkSettings, gbc_hyperlinkSettings);

        if (connectionFailed) {
            hyperlinkRetry = new JXHyperlink();
            hyperlinkRetry.setText(OCTANE_SETTINGS_RETRY);
            Font  hyperlinkRetryFont = new Font(hyperlinkRetry.getFont().getFontName(),Font.PLAIN,18);
            hyperlinkRetry.setFont(hyperlinkRetryFont);
            GridBagConstraints gbc_hyperlinkRetry = new GridBagConstraints();
            gbc_hyperlinkRetry.anchor = GridBagConstraints.CENTER;
            gbc_hyperlinkRetry.gridx = 1;
            gbc_hyperlinkRetry.gridy = 4;
            add(hyperlinkRetry, gbc_hyperlinkRetry);
        }

    }

    @Override
    public JComponent getComponent() {
        return this;
    }

}
