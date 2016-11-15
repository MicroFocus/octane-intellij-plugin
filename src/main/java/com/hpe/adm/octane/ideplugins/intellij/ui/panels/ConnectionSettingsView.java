package com.hpe.adm.octane.ideplugins.intellij.ui.panels;

import javax.swing.*;

public class ConnectionSettingsView {

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

    public JPanel getRootPanel(){
        return rootPanel;
    }

    public String getServerUrl(){
        return txtFieldServerUrl.getText();
    }

    public String getUserName(){
        return txtFieldUserName.getText();
    }

    public String getPassword(){
        return passField.getPassword().toString();
    }

    public String getWorkspace() { return txtFieldWorkspace.getText(); }

}
