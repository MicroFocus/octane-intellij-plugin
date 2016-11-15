package com.hpe.adm.octane.ideplugins.intellij.ui.panels;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.util.UrlParser;
import com.hpe.adm.octane.ideplugins.services.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.TestService;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

public class ConnectionSettingsView {

    @Inject
    private TestService testService;

    private JPanel rootPanel;
    private JLabel lblServerUrl;
    private JTextField txtFieldServerUrl;
    private JLabel lblWorkspace;
    private JTextField txtFieldWorkspace;
    private JLabel lblUserName;
    private JTextField txtFieldUserName;
    private JLabel lblPassword;
    private JPasswordField passField;
    private JButton btnTest;

    public ConnectionSettingsView() {

        //init change handlers
        txtFieldServerUrl.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                setWorkspaceFromServerUrl(txtFieldServerUrl.getText());
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                setWorkspaceFromServerUrl(txtFieldServerUrl.getText());
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                setWorkspaceFromServerUrl(txtFieldServerUrl.getText());
            }
        });

        btnTest.addActionListener(event -> {
            try {
                testService.testConnection();
                System.out.println("YES");
            } catch (Exception ex) {
                System.out.println("NOPE");
            }
        });

    }

    private void setWorkspaceFromServerUrl(String serverUrl) {
        ConnectionSettings connectionSettings = UrlParser.resolveConnectionSettings(serverUrl, getUserName(), getPassword());
        setWorkspace(connectionSettings.getWorkspaceId());
    }

    public JPanel getRootPanel(){
        return rootPanel;
    }

    public String getServerUrl(){
        return txtFieldServerUrl.getText();
    }

    public void setServerUrl(String serverUrl) {
        this.txtFieldServerUrl.setText(serverUrl);
    }

    public String getUserName(){
        return txtFieldUserName.getText();
    }

    public void setUserName(String userName) {
        this.txtFieldUserName.setText(userName);
    }

    public String getPassword(){
        return passField.getPassword().toString();
    }

    public void setPassword(String password) {
        passField.setText(password);
    }

    public void setWorkspace(Long workspaceId) {
        String workspaceText = workspaceId != null ? workspaceId.toString() : "Retrieved from URL";
        this.txtFieldWorkspace.setText(workspaceText);
    }

}
