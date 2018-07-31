/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
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

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.util.UrlParser;
import com.intellij.util.ui.JBUI;
import com.intellij.util.ui.UIUtil;
import org.apache.commons.lang.StringUtils;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

public class ConnectionSettingsComponent implements HasComponent {

    private static final String EMPTY_SERVER_URL_TEXT = "Copy paste your Octane URL from the browser here...";
    private static final String EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT = "Retrieved from server URL";
    private static final Insets insets = JBUI.insets(5);
    
    private JPanel rootPanel;
    private JTextField txtFieldServerUrl;
    private JTextField txtFieldSharedSpace;
    private JTextField txtFieldWorkspace;
    private JTextField txtFieldUserName;
    private JPasswordField passField;
    private JButton btnTest;
    private JButton btnClearSettings;
    private JLabel lblConnectionStatus;

    public ConnectionSettingsComponent() {
        rootPanel = new JPanel();
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[] {0, 0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,};
        gridBagLayout.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0};
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
        gbc_txtFieldServerUrl.gridwidth = 2;
        gbc_txtFieldServerUrl.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldServerUrl.insets = JBUI.insets(5, 5, 5, 0);
        gbc_txtFieldServerUrl.gridx = 0;
        gbc_txtFieldServerUrl.gridy = 1;
        rootPanel.add(txtFieldServerUrl, gbc_txtFieldServerUrl);
        txtFieldServerUrl.setColumns(10);

        JLabel lblSharedSpace = new JLabel("Shared space:");
        GridBagConstraints gbc_lblSharedSpace = new GridBagConstraints();
        gbc_lblSharedSpace.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblSharedSpace.insets = insets;
        gbc_lblSharedSpace.gridx = 0;
        gbc_lblSharedSpace.gridy = 2;
        rootPanel.add(lblSharedSpace, gbc_lblSharedSpace);

        txtFieldSharedSpace = new JTextField();
        txtFieldSharedSpace.setEnabled(false);
        txtFieldSharedSpace.setEditable(false);
        GridBagConstraints gbc_txtFieldSharedSpace = new GridBagConstraints();
        gbc_txtFieldSharedSpace.gridwidth = 2;
        gbc_txtFieldSharedSpace.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldSharedSpace.insets = JBUI.insets(5, 5, 5, 0);
        gbc_txtFieldSharedSpace.gridx = 0;
        gbc_txtFieldSharedSpace.gridy = 3;
        rootPanel.add(txtFieldSharedSpace, gbc_txtFieldSharedSpace);
        txtFieldSharedSpace.setColumns(10);

        JLabel lblWorkspace = new JLabel("Workspace:");
        GridBagConstraints gbc_lblWorkspace = new GridBagConstraints();
        gbc_lblWorkspace.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblWorkspace.insets = insets;
        gbc_lblWorkspace.gridx = 0;
        gbc_lblWorkspace.gridy = 4;
        rootPanel.add(lblWorkspace, gbc_lblWorkspace);

        txtFieldWorkspace = new JTextField();
        txtFieldWorkspace.setEnabled(false);
        txtFieldWorkspace.setEditable(false);
        GridBagConstraints gbc_txtFieldWorkspace = new GridBagConstraints();
        gbc_txtFieldWorkspace.gridwidth = 2;
        gbc_txtFieldWorkspace.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldWorkspace.insets = JBUI.insets(5, 5, 5, 0);
        gbc_txtFieldWorkspace.gridx = 0;
        gbc_txtFieldWorkspace.gridy = 5;
        rootPanel.add(txtFieldWorkspace, gbc_txtFieldWorkspace);
        txtFieldWorkspace.setColumns(10);

        JSeparator separator1 = new JSeparator();
        GridBagConstraints gbc_separator1 = new GridBagConstraints();
        gbc_separator1.gridwidth = 2;
        gbc_separator1.fill = GridBagConstraints.HORIZONTAL;
        gbc_separator1.insets = JBUI.insets(5, 5, 5, 0);
        gbc_separator1.gridx = 0;
        gbc_separator1.gridy = 6;
        rootPanel.add(separator1, gbc_separator1);

        JLabel lblUsername = new JLabel("Username:");
        GridBagConstraints gbc_lblUsername = new GridBagConstraints();
        gbc_lblUsername.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblUsername.insets = insets;
        gbc_lblUsername.gridx = 0;
        gbc_lblUsername.gridy = 7;
        rootPanel.add(lblUsername, gbc_lblUsername);

        txtFieldUserName = new JTextField();
        GridBagConstraints gbc_txtFieldUserName = new GridBagConstraints();
        gbc_txtFieldUserName.gridwidth = 2;
        gbc_txtFieldUserName.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldUserName.insets = JBUI.insets(5, 5, 5, 0);
        gbc_txtFieldUserName.gridx = 0;
        gbc_txtFieldUserName.gridy = 8;
        rootPanel.add(txtFieldUserName, gbc_txtFieldUserName);
        txtFieldUserName.setColumns(10);

        JLabel lblPassword = new JLabel("Password:");
        GridBagConstraints gbc_lblPassword = new GridBagConstraints();
        gbc_lblPassword.fill = GridBagConstraints.HORIZONTAL;
        gbc_lblPassword.insets = insets;
        gbc_lblPassword.gridx = 0;
        gbc_lblPassword.gridy = 9;
        rootPanel.add(lblPassword, gbc_lblPassword);

        passField = new JPasswordField();
        GridBagConstraints gbc_passField = new GridBagConstraints();
        gbc_passField.gridwidth = 2;
        gbc_passField.fill = GridBagConstraints.HORIZONTAL;
        gbc_passField.insets = JBUI.insets(5, 5, 5, 0);
        gbc_passField.gridx = 0;
        gbc_passField.gridy = 10;
        rootPanel.add(passField, gbc_passField);

        JSeparator separator2 = new JSeparator();
        GridBagConstraints gbc_separator2 = new GridBagConstraints();
        gbc_separator2.gridwidth = 2;
        gbc_separator2.fill = GridBagConstraints.HORIZONTAL;
        gbc_separator2.insets = JBUI.insets(5, 5, 5, 0);
        gbc_separator2.gridx = 0;
        gbc_separator2.gridy = 11;
        rootPanel.add(separator2, gbc_separator2);

        btnTest = new JButton("Test connection");
        GridBagConstraints gbc_btnTest = new GridBagConstraints();
        gbc_btnTest.fill = GridBagConstraints.HORIZONTAL;
        gbc_btnTest.insets = JBUI.insets(5, 0, 5, 5);
        gbc_btnTest.gridx = 0;
        gbc_btnTest.gridy = 12;
        rootPanel.add(btnTest, gbc_btnTest);
        
        lblConnectionStatus = new JLabel();
        lblConnectionStatus.setForeground(Color.RED);
        GridBagConstraints gbc_label = new GridBagConstraints();
        gbc_label.fill = GridBagConstraints.BOTH;
        gbc_label.gridwidth = 2;
        gbc_label.insets = JBUI.insets(5, 5, 5, 0);
        gbc_label.gridx = 0;
        gbc_label.gridy = 13;
        rootPanel.add(lblConnectionStatus, gbc_label);

        btnClearSettings = new JButton("Clear settings");
        GridBagConstraints gbc_btnClearSettings = new GridBagConstraints();
        gbc_btnClearSettings.fill = GridBagConstraints.HORIZONTAL;
        gbc_btnClearSettings.insets = JBUI.insets(5, 0, 5, 5);
        gbc_btnClearSettings.gridx = 0;
        gbc_btnClearSettings.gridy = 14;
        rootPanel.add(btnClearSettings, gbc_btnClearSettings);

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


}
