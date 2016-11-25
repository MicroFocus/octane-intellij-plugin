package com.hpe.adm.octane.ideplugins.intellij.ui.components;

import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.util.UrlParser;
import org.apache.commons.lang.StringUtils;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

/**
 * This class is created by and interacts with {@link com.hpe.adm.octane.ideplugins.intellij.settings.ConnectionSettingsConfigurable}
 */
public class ConnectionSettingsComponent implements HasComponent {

    private static final String EMPTY_SERVER_URL_TEXT = "Copy paste your Octane URL from the browser here...";
    private static final String EMPTY_SHAREDSPACE_WORKSPACE_URL_TEXT = "Retrieved from server URL";

    private JPanel rootPanel;
    private JTextField txtFieldServerUrl;
    private JTextField txtFieldWorkspace;
    private JTextField txtFieldUserName;
    private JPasswordField passField;
    private JButton btnTest;

    private JTextField txtFieldSharedSpace;

    private JLabel lblLoading;
    private JLabel lblConnectionStatus;
    private JButton btnClearSettings;

    public ConnectionSettingsComponent() {

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

        btnClearSettings.addActionListener((event) -> {
            setServerUrl("");
            txtFieldUserName.setText("");
            passField.setText("");
        });
    }

    /**
     * Parses
     * @param serverUrl
     */
    private void setFieldsFromServerUrl(String serverUrl) {
        if(!StringUtils.isEmpty(serverUrl) && !EMPTY_SERVER_URL_TEXT.equals(serverUrl)) {
            ConnectionSettings connectionSettings;
            try {
                connectionSettings = UrlParser.resolveConnectionSettings(serverUrl, getUserName(), getPassword());
                setConnectionStatusLabel(false);
            } catch (ServiceException ex) {
                connectionSettings = new ConnectionSettings();
                setConnectionStatusErrorLabel(ex.getMessage());
            }
            setSharedspaceWorkspaceIds(connectionSettings.getSharedSpaceId(), connectionSettings.getWorkspaceId());
        } else {
            setSharedspaceWorkspaceIds(null, null);
        }
    }

    @Override
    public JComponent getComponent(){
        rootPanel.setBorder(BorderFactory.createEmptyBorder());
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

    /**
     * Null or empty string hides the error message
     * @param errorText
     */
    public void setConnectionStatusErrorLabel(String errorText){
        lblConnectionStatus.setVisible(true);
        lblConnectionStatus.setForeground(Color.RED);
        if(!StringUtils.isEmpty(errorText)){
            lblConnectionStatus.setText(errorText);
        }
    }

    public void setConnectionStatusSuccessLabel(){
        lblConnectionStatus.setVisible(true);
        lblConnectionStatus.setForeground(new Color(0,133,0));
        lblConnectionStatus.setText("Connection successful.");
    }

    public void setConnectionStatusLabel(boolean isVisible){
        lblConnectionStatus.setVisible(isVisible);
    }

}
