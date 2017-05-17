package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.services.connection.ConnectionSettings;
import com.hpe.adm.octane.services.exception.ServiceException;
import com.hpe.adm.octane.services.util.Constants;
import com.hpe.adm.octane.services.util.UrlParser;
import com.intellij.util.ui.UIUtil;
import org.apache.commons.lang.StringUtils;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

public class TestFrameExample implements HasComponent {

    private static final String EMPTY_SERVER_URL_TEXT = "Copy paste your Octane URL from the browser here...";
    private static final String EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT = "Retrieved from server URL";

    private JPanel rootPanel;
    private JTextField txtFieldServerUrl;
    private JTextField txtFieldSharedSpace;
    private JTextField txtFieldWorkspace;
    private JTextField txtFieldUserName;
    private JPasswordField passField;
    private JButton btnTest;
    private JButton btnClearSettings;
    private JLabel lblConnectionStatus;
    private JPanel panel;


    public TestFrameExample() {
        rootPanel = new JPanel();
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{184, 0};
        gridBagLayout.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 40, 0, 0, 0, 0, 40, 0, 36, 0, 0};
        gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, Double.MIN_VALUE};
        rootPanel.setLayout(gridBagLayout);

        JLabel lblServerUrl = new JLabel("Server URL:");
        lblServerUrl.setHorizontalAlignment(SwingConstants.LEFT);
        GridBagConstraints gbc_lblServerUrl = new GridBagConstraints();
        gbc_lblServerUrl.anchor = GridBagConstraints.WEST;
        gbc_lblServerUrl.insets = new Insets(5, 5, 5, 0);
        gbc_lblServerUrl.gridx = 0;
        gbc_lblServerUrl.gridy = 0;
        rootPanel.add(lblServerUrl, gbc_lblServerUrl);

        txtFieldServerUrl = new JTextField();
        GridBagConstraints gbc_txtFieldServerUrl = new GridBagConstraints();
        gbc_txtFieldServerUrl.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldServerUrl.insets = new Insets(0, 10, 5, 10);
        gbc_txtFieldServerUrl.gridx = 0;
        gbc_txtFieldServerUrl.gridy = 1;
        rootPanel.add(txtFieldServerUrl, gbc_txtFieldServerUrl);
        txtFieldServerUrl.setColumns(10);

        JLabel lblSharedSpace = new JLabel("Shared space:");
        GridBagConstraints gbc_lblSharedSpace = new GridBagConstraints();
        gbc_lblSharedSpace.anchor = GridBagConstraints.WEST;
        gbc_lblSharedSpace.insets = new Insets(5, 5, 5, 0);
        gbc_lblSharedSpace.gridx = 0;
        gbc_lblSharedSpace.gridy = 2;
        rootPanel.add(lblSharedSpace, gbc_lblSharedSpace);

        txtFieldSharedSpace = new JTextField();
        txtFieldSharedSpace.setEnabled(false);
        txtFieldSharedSpace.setEditable(false);
        GridBagConstraints gbc_txtFieldSharedSpace = new GridBagConstraints();
        gbc_txtFieldSharedSpace.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldSharedSpace.insets = new Insets(5, 10, 5, 10);
        gbc_txtFieldSharedSpace.gridx = 0;
        gbc_txtFieldSharedSpace.gridy = 3;
        rootPanel.add(txtFieldSharedSpace, gbc_txtFieldSharedSpace);
        txtFieldSharedSpace.setColumns(10);

        JLabel lblWorkspace = new JLabel("Workspace:");
        GridBagConstraints gbc_lblWorkspace = new GridBagConstraints();
        gbc_lblWorkspace.anchor = GridBagConstraints.WEST;
        gbc_lblWorkspace.insets = new Insets(5, 5, 5, 0);
        gbc_lblWorkspace.gridx = 0;
        gbc_lblWorkspace.gridy = 4;
        rootPanel.add(lblWorkspace, gbc_lblWorkspace);

        txtFieldWorkspace = new JTextField();
        txtFieldWorkspace.setEnabled(false);
        txtFieldWorkspace.setEditable(false);
        GridBagConstraints gbc_txtFieldWorkspace = new GridBagConstraints();
        gbc_txtFieldWorkspace.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldWorkspace.insets = new Insets(5, 10, 5, 10);
        gbc_txtFieldWorkspace.gridx = 0;
        gbc_txtFieldWorkspace.gridy = 5;
        rootPanel.add(txtFieldWorkspace, gbc_txtFieldWorkspace);
        txtFieldWorkspace.setColumns(10);

        JSeparator separator = new JSeparator();
        GridBagConstraints gbc_separator = new GridBagConstraints();
        gbc_separator.anchor = GridBagConstraints.WEST;
        gbc_separator.insets = new Insets(0, 0, 5, 0);
        gbc_separator.gridx = 0;
        gbc_separator.gridy = 6;
        rootPanel.add(separator, gbc_separator);

        JLabel lblUsername = new JLabel("Username:");
        GridBagConstraints gbc_lblUsername = new GridBagConstraints();
        gbc_lblUsername.anchor = GridBagConstraints.WEST;
        gbc_lblUsername.insets = new Insets(5, 5, 5, 0);
        gbc_lblUsername.gridx = 0;
        gbc_lblUsername.gridy = 7;
        rootPanel.add(lblUsername, gbc_lblUsername);

        txtFieldUserName = new JTextField();
        GridBagConstraints gbc_txtFieldUserName = new GridBagConstraints();
        gbc_txtFieldUserName.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtFieldUserName.insets = new Insets(5, 10, 5, 10);
        gbc_txtFieldUserName.gridx = 0;
        gbc_txtFieldUserName.gridy = 8;
        rootPanel.add(txtFieldUserName, gbc_txtFieldUserName);
        txtFieldUserName.setColumns(10);

        JLabel lblPassword = new JLabel("Password:");
        GridBagConstraints gbc_lblPassword = new GridBagConstraints();
        gbc_lblPassword.anchor = GridBagConstraints.WEST;
        gbc_lblPassword.insets = new Insets(5, 5, 5, 0);
        gbc_lblPassword.gridx = 0;
        gbc_lblPassword.gridy = 9;
        rootPanel.add(lblPassword, gbc_lblPassword);

        passField = new JPasswordField();
        GridBagConstraints gbc_passField = new GridBagConstraints();
        gbc_passField.fill = GridBagConstraints.HORIZONTAL;
        gbc_passField.insets = new Insets(5, 10, 5, 10);
        gbc_passField.gridx = 0;
        gbc_passField.gridy = 10;
        rootPanel.add(passField, gbc_passField);

        JSeparator separator_1 = new JSeparator();
        GridBagConstraints gbc_separator_1 = new GridBagConstraints();
        gbc_separator_1.anchor = GridBagConstraints.WEST;
        gbc_separator_1.insets = new Insets(0, 0, 5, 0);
        gbc_separator_1.gridx = 0;
        gbc_separator_1.gridy = 11;
        rootPanel.add(separator_1, gbc_separator_1);

        btnTest = new JButton("Test connection");
        GridBagConstraints gbc_btnTest = new GridBagConstraints();
        gbc_btnTest.anchor = GridBagConstraints.WEST;
        gbc_btnTest.insets = new Insets(5, 5, 5, 0);
        gbc_btnTest.gridx = 0;
        gbc_btnTest.gridy = 12;
        rootPanel.add(btnTest, gbc_btnTest);

        panel = new JPanel();
        GridBagConstraints gbc_panel = new GridBagConstraints();
        gbc_panel.fill = GridBagConstraints.BOTH;
        gbc_panel.insets = new Insets(5, 10, 5, 10);
        gbc_panel.gridx = 0;
        gbc_panel.gridy = 13;
        rootPanel.add(panel, gbc_panel);
        GridBagLayout gbl_panel = new GridBagLayout();
        gbl_panel.columnWidths = new int[]{184, 0};
        gbl_panel.rowHeights = new int[]{0, 0};
        gbl_panel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_panel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
        panel.setLayout(gbl_panel);

        lblConnectionStatus = new JLabel();
        GridBagConstraints gbc_lblConnectionStatus = new GridBagConstraints();
        gbc_lblConnectionStatus.gridx = 0;
        gbc_lblConnectionStatus.gridy = 0;
        panel.add(lblConnectionStatus, gbc_lblConnectionStatus);
        lblConnectionStatus.setOpaque(true);
        lblConnectionStatus.setFocusable(false);

        btnClearSettings = new JButton("Clear settings");
        GridBagConstraints gbc_btnClearSettings = new GridBagConstraints();
        gbc_btnClearSettings.anchor = GridBagConstraints.WEST;
        gbc_btnClearSettings.insets = new Insets(5, 5, 0, 0);
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
                if (txtFieldServerUrl.getText().trim().length() == 0) {
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
     * Parses
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
                setConnectionStatusError(ex.getMessage() + "<br>" + Constants.CORRECT_URL_FORMAT_MESSAGE);
            }
            setSharedspaceWorkspaceIds(connectionSettings.getSharedSpaceId(), connectionSettings.getWorkspaceId());
        } else {
            setSharedspaceWorkspaceIds(null, null);
        }
    }

    @Override
    public JComponent getComponent() {
        rootPanel.setBorder(BorderFactory.createEmptyBorder());
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
     *
     * @param errorText can be html
     */
    public void setConnectionStatusError(String errorText) {
        lblConnectionStatus.setVisible(true);
        setTestConnectionButtonEnabled(true);
        lblConnectionStatus.setForeground(Color.RED);
        if (!StringUtils.isEmpty(errorText)) {
            lblConnectionStatus.setText(errorText);
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

        // lblConnectionStatus.setText("Connection successful.");
    }

    /**
     * Disables the test connection button, sets a waiting message for the label
     */
    public void setConnectionStatusLoading() {
        lblConnectionStatus.setVisible(true);
        lblConnectionStatus.setForeground(UIUtil.getLabelForeground());
        //lblConnectionStatus.setText("Testing connection, please wait.");
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
                //btnTest.requestFocus();
            }
        });
    }


}
