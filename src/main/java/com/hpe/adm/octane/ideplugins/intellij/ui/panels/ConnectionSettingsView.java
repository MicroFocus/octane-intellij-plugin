package com.hpe.adm.octane.ideplugins.intellij.ui.panels;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.util.UrlParser;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import org.apache.commons.lang.StringUtils;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

public class ConnectionSettingsView {

    private static final String EMPTY_SERVER_URL_TEXT = "Copy paste your Octane URL from the browser here...";
    private static final String EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT = "Retrieved from server URL";

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

    private JLabel lblSharedSpaceUrl;
    private JTextField txtFieldSharedSpace;

    private JLabel lblLoading;

    @Inject
    private ConnectionSettings connectionSettings;

    public ConnectionSettingsView() {

        //Default placeholder
        txtFieldSharedSpace.setText(EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT);
        txtFieldWorkspace.setText(EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT);

        lblLoading.setVisible(false);

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
                if(EMPTY_SERVER_URL_TEXT.equals(txtFieldServerUrl.getText())){
                    txtFieldServerUrl.setText("");
                }
            }
            @Override
            public void focusLost(FocusEvent e) {
                if(txtFieldServerUrl.getText().trim().length()==0){
                    txtFieldServerUrl.setText(EMPTY_SERVER_URL_TEXT);
                }
            }
        });

    }

    private void setFieldsFromServerUrl(String serverUrl) {
        if(!StringUtils.isEmpty(serverUrl) && !EMPTY_SERVER_URL_TEXT.equals(serverUrl)) {
            ConnectionSettings connectionSettings = UrlParser.resolveConnectionSettings(serverUrl, getUserName(), getPassword());
            setSharedspaceWorkspaceIds(connectionSettings.getSharedSpaceId(), connectionSettings.getWorkspaceId());
        }
    }

    public JPanel getRootPanel(){
        return rootPanel;
    }

    public String getServerUrl(){
        return EMPTY_SERVER_URL_TEXT.equals(txtFieldServerUrl.getText()) ? null : txtFieldServerUrl.getText();
    }

    public void setServerUrl(String serverUrl) {
        String serverUrlText = !StringUtils.isEmpty(serverUrl) ? serverUrl : EMPTY_SERVER_URL_TEXT;
        this.txtFieldServerUrl.setText(serverUrlText);
    }

    public String getUserName(){
        return txtFieldUserName.getText();
    }

    public void setUserName(String userName) {
        this.txtFieldUserName.setText(userName);
    }

    public String getPassword(){
        return new String(passField.getPassword());
    }

    public void setPassword(String password) {
        passField.setText(password);
    }

    //This is private, should always be set from the base url
    private void setSharedspaceWorkspaceIds(Long sharedspaceId, Long workspaceId) {
        String sharedspaceText = sharedspaceId != null ? sharedspaceId.toString() : EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT;
        this.txtFieldSharedSpace.setText(sharedspaceText);
        String workspaceText = workspaceId != null ? workspaceId.toString() : EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT;
        this.txtFieldWorkspace.setText(workspaceText);
    }

    public void setTestConnectionActionListener(ActionListener actionListener) {
        btnTest.addActionListener(actionListener);
    }

    public void isLoading(boolean isLoading){
        lblLoading.setVisible(isLoading);
    }

}
