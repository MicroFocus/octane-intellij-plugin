/*******************************************************************************
 * Copyright 2017-2026 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors ("Open Text") are as may be set forth
 * in the express warranty statements accompanying such products and services.
 * Nothing herein should be construed as constituting an additional warranty.
 * Open Text shall not be liable for technical or editorial errors or
 * omissions contained herein. The information contained herein is subject
 * to change without notice.
 *
 * Except as specifically indicated otherwise, this document contains
 * confidential information and a valid license is required for possession,
 * use or copying. If this work is provided to the U.S. Government,
 * consistent with FAR 12.211 and 12.212, Commercial Computer Software,
 * Computer Software Documentation, and Technical Data for Commercial Items are
 * licensed to the U.S. Government under vendor's standard commercial license.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package com.hpe.adm.octane.ideplugins.intellij.ui.components;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork.NoWorkPanel;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.util.Map;

public class WelcomeViewComponent extends JPanel implements HasComponent {

    private static final long serialVersionUID = 1L;
    private static final String WELCOME_TEXT = "Welcome to Core Software Delivery Platform plugin";
    private static final String OCTANE_SETTINGS_TEXT = "To start, go to Settings and connect.";
    private static final String OCTANE_SETTINGS_RETRY = "Retry";

    private static final Color HYPERLINK_COLOR = new Color(3, 93, 185);
    private static final Cursor CURSOR = new Cursor(Cursor.HAND_CURSOR);

    private JLabel hyperlinkSettings;
    private JLabel hyperlinkRetry;
    private JLabel lblMessage;
    private boolean connectionFailed;

    public WelcomeViewComponent(Project project) {
        init(project);
    }

    public WelcomeViewComponent(Project project, String welcomeMessage, String settingsLinkMessage, String retryMessage) {
        final ConnectionSettingsProvider connectionSettingsProvider = PluginModule.getPluginModuleForProject(project).getInstance(ConnectionSettingsProvider.class);
        connectionFailed = retryMessage != null;
        init(project);
        lblMessage.setText(welcomeMessage);
        hyperlinkSettings.setText(settingsLinkMessage);
        if (connectionFailed) {
            hyperlinkRetry.setText(retryMessage);
        }
    }

    /**
     * Create the panel.
     */
    private void init(Project project) {
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0, 0, 0, 0, 0};
        gridBagLayout.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{0.0, 1.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
        setLayout(gridBagLayout);

        addCompanyLabel();
        addOctaneLogoLabel();
        addWelcomeTextLabel();
        addSettingsHyperlinkLabel(project);

        if (connectionFailed) {
            addRetryHyperlinkLabel(project);
        }
    }

    private void addCompanyLabel() {
        JLabel lblCompany = new JLabel("");
        lblCompany.setIcon(new ImageIcon(NoWorkPanel.class.getResource(Constants.IMG_VENDOR_LOGO_LIGHT)));

        GridBagConstraints gbc_lblCompany = new GridBagConstraints();
        gbc_lblCompany.anchor = GridBagConstraints.SOUTHEAST;
        gbc_lblCompany.fill = GridBagConstraints.VERTICAL;
        gbc_lblCompany.insets = new Insets(0, 0, 10, 10);
        gbc_lblCompany.gridx = 1;
        gbc_lblCompany.gridy = 5;

        add(lblCompany, gbc_lblCompany);
    }

    private void addOctaneLogoLabel() {
        JLabel lblOctane = new JLabel("");
        lblOctane.setIcon(new ImageIcon(NoWorkPanel.class.getResource(Constants.IMG_OCTANE_LOGO)));

        GridBagConstraints gbc_lblOctane = new GridBagConstraints();
        gbc_lblOctane.anchor = GridBagConstraints.CENTER;
        gbc_lblOctane.fill = GridBagConstraints.VERTICAL;
        gbc_lblOctane.insets = new Insets(10, 0, 0, 0);
        gbc_lblOctane.gridx = 1;
        gbc_lblOctane.gridy = 1;

        add(lblOctane, gbc_lblOctane);
    }

    private void addWelcomeTextLabel() {
        lblMessage = new JLabel(WELCOME_TEXT);

        Font lblMessageFont = new Font(lblMessage.getFont().getFontName(), Font.PLAIN, 18);
        lblMessage.setFont(lblMessageFont);

        GridBagConstraints gbc_lblMessage = new GridBagConstraints();
        gbc_lblMessage.anchor = GridBagConstraints.CENTER;
        gbc_lblMessage.insets = new Insets(0, 0, 5, 0);
        gbc_lblMessage.gridx = 1;
        gbc_lblMessage.gridy = 2;
        add(lblMessage, gbc_lblMessage);
    }

    private void addSettingsHyperlinkLabel(Project project) {
        hyperlinkSettings = new JLabel();
        hyperlinkSettings.setText(OCTANE_SETTINGS_TEXT);
        hyperlinkSettings.setForeground(HYPERLINK_COLOR);
        hyperlinkSettings.setCursor(CURSOR);

        Font hyperlinkSettingsFont = new Font(hyperlinkSettings.getFont().getFontName(), Font.PLAIN, 18);
        hyperlinkSettings.setFont(hyperlinkSettingsFont);

        hyperlinkSettings.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                ShowSettingsUtil.getInstance().showSettingsDialog(project, ConnectionSettingsConfigurable.class);
            }

            @Override
            public void mouseExited(MouseEvent e) {
                setUnderlineText(hyperlinkSettings, TextAttribute.SUPERSCRIPT_SUB);
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                setUnderlineText(hyperlinkSettings, TextAttribute.UNDERLINE_ON);
            }
        });
        GridBagConstraints gbc_hyperlinkSettings = new GridBagConstraints();
        gbc_hyperlinkSettings.anchor = GridBagConstraints.CENTER;
        gbc_hyperlinkSettings.insets = new Insets(0, 0, 5, 0);
        gbc_hyperlinkSettings.gridx = 1;
        gbc_hyperlinkSettings.gridy = 3;

        add(hyperlinkSettings, gbc_hyperlinkSettings);
    }

    private void addRetryHyperlinkLabel(Project project) {
        hyperlinkRetry = new JLabel();
        hyperlinkRetry.setText(OCTANE_SETTINGS_RETRY);
        hyperlinkRetry.setForeground(HYPERLINK_COLOR);
        hyperlinkRetry.setCursor(CURSOR);

        Font hyperlinkRetryFont = new Font(hyperlinkRetry.getFont().getFontName(), Font.PLAIN, 18);
        hyperlinkRetry.setFont(hyperlinkRetryFont);

        hyperlinkRetry.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                ShowSettingsUtil.getInstance().showSettingsDialog(project, ConnectionSettingsConfigurable.class);
            }

            @Override
            public void mouseExited(MouseEvent e) {
                setUnderlineText(hyperlinkRetry, TextAttribute.SUPERSCRIPT_SUB);
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                setUnderlineText(hyperlinkRetry, TextAttribute.UNDERLINE_ON);
            }
        });

        GridBagConstraints gbc_hyperlinkRetry = new GridBagConstraints();
        gbc_hyperlinkRetry.anchor = GridBagConstraints.CENTER;
        gbc_hyperlinkRetry.gridx = 1;
        gbc_hyperlinkRetry.gridy = 4;

        add(hyperlinkRetry, gbc_hyperlinkRetry);
    }

    @Override
    public JComponent getComponent() {
        return this;
    }

    private void setUnderlineText(JLabel label, Integer underline) {
        Font font = label.getFont();
        Map<TextAttribute, Object> attributes = (Map<TextAttribute, Object>) font.getAttributes();
        attributes.put(TextAttribute.UNDERLINE, underline);
        label.setFont(font.deriveFont(attributes));
    }
}
