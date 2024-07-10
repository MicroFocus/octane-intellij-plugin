/*******************************************************************************
 * Copyright 2017-2023 Open Text.
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

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.util.UrlParser;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.util.Pair;
import com.intellij.util.net.HttpConfigurable;
import com.intellij.util.ui.JBUI;
import com.intellij.util.ui.UIUtil;
import org.apache.commons.lang3.StringUtils;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class ConnectionSettingsComponent implements HasComponent {

    private static final String EMPTY_SERVER_URL_TEXT = "Copy paste your Core Software Delivery Platform URL from the browser here...";
    private static final String EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT = "Retrieved from server URL";

    private static final String userPassAuthInfoText = "Log into Core Software Delivery Platform directly with your user name and password, in non-SSO environments. " + System.lineSeparator() +
            "This method saves your login credentials between sessions, so you do not have to re-enter them.";

    private static final String browserAuthInfoText = "Log into Core Software Delivery Platform using a browser. " + System.lineSeparator() +
            "You can use this method for non-SSO, SSO, and federated environments. " + System.lineSeparator() +
            "Your login credentials are not saved between sessions, so you will have to re-enter them each time.";

    private JPanel rootPanel;
    private JTextField txtFieldServerUrl;
    private JTextField txtFieldSharedSpace;
    private JTextField txtFieldWorkspace;
    private JTextField txtFieldUserName;
    private JPasswordField passField;
    private JButton btnTest;
    private JButton btnClearSettings;
    private JLabel lblConnectionStatus;
    private JRadioButton userPassButton;
    private JRadioButton ssoRadioButton;
    private JXHyperlink hyperlinkResetUser;
    private JLabel lblProxySettings;

    private Project project;

    public ConnectionSettingsComponent(Project project) {
        this.project = project;
        rootPanel = new JPanel();
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWeights = new double[]{0.0, 1.0};
        gridBagLayout.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0};
        rootPanel.setLayout(gridBagLayout);

        JLabel lblServerUrl = new JLabel("Server URL:");
        lblServerUrl.setHorizontalAlignment(SwingConstants.LEFT);
        GridBagConstraints gbc_lblServerUrl = new GridBagConstraints();
        gbc_lblServerUrl.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblServerUrl.insets = JBUI.insets(5);
        gbc_lblServerUrl.gridx = 0;
        gbc_lblServerUrl.gridy = 0;
        rootPanel.add(lblServerUrl, gbc_lblServerUrl);

        txtFieldServerUrl = new JTextField();
        GridBagConstraints gbc_txtFieldServerUrl = new GridBagConstraints();
        gbc_txtFieldServerUrl.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldServerUrl.insets = JBUI.insets(5, 5, 5, 0);
        gbc_txtFieldServerUrl.gridx = 1;
        gbc_txtFieldServerUrl.gridy = 0;
        rootPanel.add(txtFieldServerUrl, gbc_txtFieldServerUrl);
        txtFieldServerUrl.setColumns(10);

        JLabel lblSharedSpace = new JLabel("Space:");
        GridBagConstraints gbc_lblSharedSpace = new GridBagConstraints();
        gbc_lblSharedSpace.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblSharedSpace.insets = JBUI.insets(5);
        gbc_lblSharedSpace.gridx = 0;
        gbc_lblSharedSpace.gridy = 2;
        rootPanel.add(lblSharedSpace, gbc_lblSharedSpace);

        txtFieldSharedSpace = new JTextField();
        txtFieldSharedSpace.setEnabled(false);
        txtFieldSharedSpace.setEditable(false);
        GridBagConstraints gbc_txtFieldSharedSpace = new GridBagConstraints();
        gbc_txtFieldSharedSpace.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldSharedSpace.insets = JBUI.insets(5, 5, 5, 0);
        gbc_txtFieldSharedSpace.gridx = 1;
        gbc_txtFieldSharedSpace.gridy = 2;
        rootPanel.add(txtFieldSharedSpace, gbc_txtFieldSharedSpace);
        txtFieldSharedSpace.setColumns(10);

        JLabel lblWorkspace = new JLabel("Workspace:");
        GridBagConstraints gbc_lblWorkspace = new GridBagConstraints();
        gbc_lblWorkspace.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblWorkspace.insets = JBUI.insets(5);
        gbc_lblWorkspace.gridx = 0;
        gbc_lblWorkspace.gridy = 3;
        rootPanel.add(lblWorkspace, gbc_lblWorkspace);

        txtFieldWorkspace = new JTextField();
        txtFieldWorkspace.setEnabled(false);
        txtFieldWorkspace.setEditable(false);
        txtFieldWorkspace.setColumns(10);
        GridBagConstraints gbc_txtFieldWorkspace = new GridBagConstraints();
        gbc_txtFieldWorkspace.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldWorkspace.insets = JBUI.insets(5, 5, 15, 0);
        gbc_txtFieldWorkspace.gridx = 1;
        gbc_txtFieldWorkspace.gridy = 3;
        rootPanel.add(txtFieldWorkspace, gbc_txtFieldWorkspace);

        addSeparator(rootPanel, 4, "Authentication");

        JPanel panelUserPassAuth = new JPanel(new FlowLayout());
        GridBagConstraints gbc_panelUserPassAuth = new GridBagConstraints();
        gbc_panelUserPassAuth.anchor = GridBagConstraints.WEST;
        gbc_panelUserPassAuth.insets = JBUI.insets(0);
        gbc_panelUserPassAuth.gridx = 0;
        gbc_panelUserPassAuth.gridy = 5;
        gbc_panelUserPassAuth.gridwidth = 2;
        rootPanel.add(panelUserPassAuth, gbc_panelUserPassAuth);

        userPassButton = new JRadioButton("Login with username and password");

        JLabel lblUserPassAuthInfo = new JLabel(IconLoader.findIcon(Constants.IMG_HELP_ICON, ConnectionSettingsComponent.class.getClassLoader()));
        lblUserPassAuthInfo.setToolTipText(userPassAuthInfoText);
        lblUserPassAuthInfo.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                JOptionPane.showMessageDialog(rootPanel, userPassAuthInfoText, "Login with username and password", JOptionPane.INFORMATION_MESSAGE);
            }
        });
        panelUserPassAuth.add(userPassButton);
        panelUserPassAuth.add(lblUserPassAuthInfo);

        JLabel lblUsername = new JLabel("Username:");
        GridBagConstraints gbc_lblUsername = new GridBagConstraints();
        gbc_lblUsername.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblUsername.insets = JBUI.insets(5, 25, 5, 0);
        gbc_lblUsername.gridx = 0;
        gbc_lblUsername.gridy = 6;
        rootPanel.add(lblUsername, gbc_lblUsername);

        txtFieldUserName = new JTextField();
        GridBagConstraints gbc_txtFieldUserName = new GridBagConstraints();
        gbc_txtFieldUserName.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldUserName.insets = JBUI.insets(5, 15, 5, 0);
        gbc_txtFieldUserName.gridx = 1;
        gbc_txtFieldUserName.gridy = 6;
        rootPanel.add(txtFieldUserName, gbc_txtFieldUserName);
        txtFieldUserName.setColumns(10);

        JLabel lblPassword = new JLabel("Password:");
        GridBagConstraints gbc_lblPassword = new GridBagConstraints();
        gbc_lblPassword.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblPassword.insets = JBUI.insets(5, 25, 5, 0);
        gbc_lblPassword.gridx = 0;
        gbc_lblPassword.gridy = 7;
        rootPanel.add(lblPassword, gbc_lblPassword);

        passField = new JPasswordField();
        GridBagConstraints gbc_passField = new GridBagConstraints();
        gbc_passField.fill = GridBagConstraints.HORIZONTAL;
        gbc_passField.insets = JBUI.insets(5, 15, 5, 0);
        gbc_passField.gridx = 1;
        gbc_passField.gridy = 7;
        rootPanel.add(passField, gbc_passField);

        JPanel panelBrowserAuth = new JPanel(new FlowLayout());
        GridBagConstraints gbc_panelBrowserAuth = new GridBagConstraints();
        gbc_panelBrowserAuth.anchor = GridBagConstraints.WEST;
        gbc_panelBrowserAuth.insets = JBUI.insets(0);
        gbc_panelBrowserAuth.gridx = 0;
        gbc_panelBrowserAuth.gridy = 8;
        gbc_panelBrowserAuth.gridwidth = 2;
        rootPanel.add(panelBrowserAuth, gbc_panelBrowserAuth);

        ssoRadioButton = new JRadioButton("Login using a browser");

        JLabel lblBrowserAuthInfo = new JLabel(IconLoader.findIcon(Constants.IMG_HELP_ICON, ConnectionSettingsComponent.class.getClassLoader()));
        lblBrowserAuthInfo.setToolTipText(browserAuthInfoText);
        lblBrowserAuthInfo.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                JOptionPane.showMessageDialog(rootPanel, browserAuthInfoText, "Login using a browser", JOptionPane.INFORMATION_MESSAGE);
            }
        });

        hyperlinkResetUser = new JXHyperlink();
        hyperlinkResetUser.setText("(Change user)");

        panelBrowserAuth.add(ssoRadioButton);
        panelBrowserAuth.add(hyperlinkResetUser);
        panelBrowserAuth.add(lblBrowserAuthInfo);

        addSeparator(rootPanel, 9);

        JPanel panelTestConnection = new JPanel();
        GridBagConstraints gbc_panelTestConnection = new GridBagConstraints();
        gbc_panelTestConnection.fill = GridBagConstraints.HORIZONTAL;
        gbc_panelTestConnection.anchor = GridBagConstraints.NORTHWEST;
        gbc_panelTestConnection.insets = JBUI.insets(5, 0, 5, 5);
        gbc_panelTestConnection.gridx = 0;
        gbc_panelTestConnection.gridy = 10;
        gbc_panelTestConnection.gridwidth = 2;
        rootPanel.add(panelTestConnection, gbc_panelTestConnection);

        GridBagLayout panelTestConnectionGridBagLayout = new GridBagLayout();
        panelTestConnectionGridBagLayout.columnWidths = new int[]{150, 0};
        panelTestConnectionGridBagLayout.columnWeights = new double[]{0.0, 1.0};
        panelTestConnection.setLayout(panelTestConnectionGridBagLayout);

        JXHyperlink hyperlinkSettings = new JXHyperlink();
        hyperlinkSettings.setText("Global IDE proxy settings");
        hyperlinkSettings.addActionListener(event -> HttpConfigurable.editConfigurable(null));
        GridBagConstraints gbc_hyperlinkSettings = new GridBagConstraints();
        gbc_hyperlinkSettings.insets = JBUI.insets(0, 5, 5, 5);
        gbc_hyperlinkSettings.fill = GridBagConstraints.HORIZONTAL;
        gbc_hyperlinkSettings.gridx = 0;
        gbc_hyperlinkSettings.gridy = 0;
        gbc_hyperlinkSettings.gridwidth = 2;
        panelTestConnection.add(hyperlinkSettings, gbc_hyperlinkSettings);

        lblProxySettings = new JLabel();
        GridBagConstraints gbc_lblProxySettings = new GridBagConstraints();
        gbc_lblProxySettings.insets = JBUI.insets(0, 5, 5, 5);
        gbc_lblProxySettings.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblProxySettings.gridx = 0;
        gbc_lblProxySettings.gridy = 1;
        gbc_lblProxySettings.gridwidth = 2;
        panelTestConnection.add(lblProxySettings, gbc_lblProxySettings);

        btnTest = new JButton("Test connection");
        GridBagConstraints gbc_btnTest = new GridBagConstraints();
        gbc_btnTest.anchor = GridBagConstraints.NORTHWEST;
        gbc_btnTest.fill = GridBagConstraints.HORIZONTAL;
        gbc_btnTest.gridx = 0;
        gbc_btnTest.gridy = 2;

        panelTestConnection.add(btnTest, gbc_btnTest);

        lblConnectionStatus = new JLabel();
        GridBagConstraints gbc_lblConnectionStatus = new GridBagConstraints();
        gbc_lblConnectionStatus.insets = JBUI.insets(0);
        gbc_lblConnectionStatus.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblConnectionStatus.gridx = 1;
        gbc_lblConnectionStatus.gridy = 2;
        gbc_lblConnectionStatus.insets = JBUI.insets(0, 5, 0, 0);
        panelTestConnection.add(lblConnectionStatus, gbc_lblConnectionStatus);

        addSeparator(panelTestConnection, 3);

        btnClearSettings = new JButton("Clear settings");
        GridBagConstraints gbc_btnClearSettings = new GridBagConstraints();
        gbc_btnClearSettings.insets = JBUI.insets(5, 0);
        gbc_btnClearSettings.fill = GridBagConstraints.HORIZONTAL;
        gbc_btnClearSettings.gridx = 0;
        gbc_btnClearSettings.gridy = 4;
        panelTestConnection.add(btnClearSettings, gbc_btnClearSettings);

        ButtonGroup buttonGroup = new ButtonGroup();
        buttonGroup.add(userPassButton);
        buttonGroup.add(ssoRadioButton);

        userPassButton.addItemListener(e -> {
            if (userPassButton.isSelected()) {
                txtFieldUserName.setEnabled(true);
                passField.setEnabled(true);
                hyperlinkResetUser.setEnabled(false);
            } else {
                txtFieldUserName.setEnabled(false);
                passField.setEnabled(false);
                hyperlinkResetUser.setEnabled(true);
            }
        });

        ssoRadioButton.setSelected(true);
        userPassButton.setSelected(true);

        //Default placeholder
        txtFieldSharedSpace.setText(EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT);
        txtFieldWorkspace.setText(EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT);

        txtFieldServerUrl.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                setFieldsFromServerUrl(txtFieldServerUrl.getText());
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                setFieldsFromServerUrl(txtFieldServerUrl.getText());
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                setFieldsFromServerUrl(txtFieldServerUrl.getText());
            }
        });

        //This focus listener will add and remove the default text when a proper url is not in place
        txtFieldServerUrl.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(FocusEvent e) {
                if (EMPTY_SERVER_URL_TEXT.equals(txtFieldServerUrl.getText())) {
                    txtFieldServerUrl.setText("");
                }
            }

            @Override
            public void focusLost(FocusEvent e) {
                if (0 == txtFieldServerUrl.getText().trim().length()) {
                    txtFieldServerUrl.setText(EMPTY_SERVER_URL_TEXT);
                }
            }
        });

        btnClearSettings.addActionListener((event) -> {
            setServerUrl("");
            txtFieldUserName.setText("");
            passField.setText("");
            lblConnectionStatus.setText("");
        });

        rootPanel.setVisible(true);
    }

    public void addResetUserActionListener(ActionListener l) {
        hyperlinkResetUser.addActionListener(l);
    }

    private static void addSeparator(JPanel panel, int gridY) {
        addSeparator(panel, gridY, null);
    }

    private static void addSeparator(JPanel panel, int gridY, String title) {
        JPanel childPanel = new JPanel();
        GridBagConstraints gbc_childPanel = new GridBagConstraints();
        gbc_childPanel.gridwidth = 2;
        gbc_childPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_childPanel.gridx = 0;
        gbc_childPanel.gridy = gridY;

        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{0.0, 0.0, 0.0, Double.MIN_VALUE};
        childPanel.setLayout(gridBagLayout);

        if (!StringUtils.isEmpty(title)) {
            JLabel lblALabel = new JLabel(title);
            GridBagConstraints gbc_lblALabel = new GridBagConstraints();
            gbc_lblALabel.anchor = GridBagConstraints.WEST;
            gbc_lblALabel.insets = JBUI.insets(0, 5, 0, 5);
            gbc_lblALabel.gridx = 0;
            gbc_lblALabel.gridy = 0;
            childPanel.add(lblALabel, gbc_lblALabel);
        }

        JSeparator separator = new JSeparator();
        GridBagConstraints gbc_separator = new GridBagConstraints();
        gbc_separator.fill = GridBagConstraints.HORIZONTAL;
        gbc_separator.insets = JBUI.insets(5, 0, 5, 0);
        gbc_separator.gridx = 1;
        gbc_separator.gridy = 0;
        childPanel.add(separator, gbc_separator);
        separator.setAlignmentX(Component.LEFT_ALIGNMENT);

        panel.add(childPanel, gbc_childPanel);
    }

    /**
     * Parses server url into the ui fields from base url, workspace, sharedspace
     *
     * @param serverUrl
     */
    private void setFieldsFromServerUrl(String serverUrl) {
        if (!StringUtils.isEmpty(serverUrl) && !EMPTY_SERVER_URL_TEXT.equals(serverUrl)) {
            ConnectionSettings connectionSettings;
            try {
                connectionSettings = UrlParser.resolveConnectionSettings(serverUrl, getUserName(), getPassword());
                setConnectionStatusLabelVisible(false);
            } catch (ServiceException ex) {
                connectionSettings = new ConnectionSettings();
                setConnectionStatusError(ex.getMessage() + " " + Constants.CORRECT_URL_FORMAT_MESSAGE);
            }
            setSharedspaceWorkspaceIds(connectionSettings.getSharedSpaceId(), connectionSettings.getWorkspaceId());
        } else {
            setSharedspaceWorkspaceIds(null, null);
        }
    }

    @Override
    public JComponent getComponent() {
        rootPanel.setBorder(JBUI.Borders.empty(10));
        return rootPanel;
    }

    public String getServerUrl() {
        return EMPTY_SERVER_URL_TEXT.equals(txtFieldServerUrl.getText()) ? null : txtFieldServerUrl.getText();
    }

    public void setServerUrl(String serverUrl) {
        String serverUrlText = !StringUtils.isEmpty(serverUrl) ? serverUrl : EMPTY_SERVER_URL_TEXT;
        txtFieldServerUrl.setText(serverUrlText);
    }

    public String getUserName() {
        return txtFieldUserName.getText();
    }

    public void setUserName(String userName) {
        txtFieldUserName.setText(userName);
    }

    public String getPassword() {
        return new String(passField.getPassword());
    }

    public void setPassword(String password) {
        passField.setText(password);
    }

    public boolean isSsoAuth() {
        return ssoRadioButton.isSelected();
    }

    public void setSsoAuth(boolean isSsoAuth) {
        if (isSsoAuth) {
            txtFieldUserName.setText("");
            passField.setText("");
        }

        txtFieldUserName.setEnabled(!isSsoAuth);
        passField.setEnabled(!isSsoAuth);
        userPassButton.setSelected(!isSsoAuth);
        ssoRadioButton.setSelected(isSsoAuth);
    }

    //This is private, should always be set from the base url
    private void setSharedspaceWorkspaceIds(Long sharedspaceId, Long workspaceId) {
        String sharedspaceText = sharedspaceId != null ? sharedspaceId.toString() : EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT;
        txtFieldSharedSpace.setText(sharedspaceText);
        String workspaceText = workspaceId != null ? workspaceId.toString() : EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT;
        txtFieldWorkspace.setText(workspaceText);
    }

    public void setTestConnectionActionListener(ActionListener actionListener) {
        btnTest.addActionListener(actionListener);
    }

    /**
     * Re-enables the test connection button, sets error text for the label
     **/
    public void setConnectionStatusError(String errorText) {
        lblConnectionStatus.setVisible(true);
        setTestConnectionButtonEnabled(true);
        if (!StringUtils.isEmpty(errorText)) {
            lblConnectionStatus.setText(errorText);
            lblConnectionStatus.setText(String.format("<html><font color='red'>%s</font></html>", errorText));
        }
    }

    /**
     * Re-enables the test connection button, sets success test for the label
     */
    public void setConnectionStatusSuccess() {
        lblConnectionStatus.setVisible(true);
        setTestConnectionButtonEnabled(true);
        lblConnectionStatus.setForeground(new Color(0, 133, 0));
        lblConnectionStatus.setText("Connection successful.");
    }

    /**
     * Disables the test connection button, sets a waiting message for the label
     */
    public void setConnectionStatusLoading() {
        lblConnectionStatus.setVisible(true);
        lblConnectionStatus.setForeground(UIUtil.getLabelForeground());
        lblConnectionStatus.setText("Testing connection, please wait.");
        btnTest.setEnabled(false);
    }

    public void setConnectionStatusLabelVisible(boolean isVisible) {
        lblConnectionStatus.setVisible(isVisible);
    }

    public void setTestConnectionButtonEnabled(boolean isEnabled) {
        SwingUtilities.invokeLater(() -> {
            if (btnTest.isEnabled() != isEnabled) {
                btnTest.setEnabled(isEnabled);
                btnTest.requestFocus();
            }
        });
    }

    public void refreshProxySettingsLabel() {

        URI uri;
        try {
            uri = new URI(txtFieldServerUrl.getText());
        } catch (URISyntaxException e) {
            return;
        }

        List<Pair<String, String>> proxySettings;
        try {
            proxySettings = HttpConfigurable.getInstance().getJvmProperties(true, uri);
        } catch (Exception ex) {
            proxySettings = Collections.emptyList();
        }

        String proxyValues = "<b>Detected proxy for current server url:</b><br>";

        if (proxySettings.size() == 0) {
            proxyValues += "No proxy detected.";

        } else {
            proxyValues +=
                    proxySettings.stream()
                            .map(pair -> pair.getFirst() + ": " + pair.getSecond())
                            .collect(Collectors.joining("<br>"));
        }

        this.lblProxySettings.setText("<html>" + proxyValues + "</html>");
    }

}
