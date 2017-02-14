package com.hpe.adm.octane.ideplugins.intellij.ui.components;

import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.services.util.Constants;
import com.hpe.adm.octane.services.connection.ConnectionSettings;
import com.hpe.adm.octane.services.exception.ServiceException;
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

    private JLabel lblConnectionStatus;
    private JButton btnClearSettings;

    public ConnectionSettingsComponent() {

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
            lblConnectionStatus.setText("");
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
     * Re-enables the test connection button, sets error text for the label
     * @param errorText can be html
     */
    public void setConnectionStatusError(String errorText){
        lblConnectionStatus.setVisible(true);
        setTestConnectionButtonEnabled(true);
        lblConnectionStatus.setForeground(Color.RED);
        if(!StringUtils.isEmpty(errorText)){
            lblConnectionStatus.setText("<html>"+errorText+"</html>");
        }
    }

    /**
     * Re-enables the test connection button, sets success test for the label
     */
    public void setConnectionStatusSuccess(){
        lblConnectionStatus.setVisible(true);
        setTestConnectionButtonEnabled(true);
        lblConnectionStatus.setForeground(new Color(0,133,0));
        lblConnectionStatus.setText("Connection successful.");
    }

    /**
     * Disables the test connection button, sets a waiting message for the label
     */
    public void setConnectionStatusLoading(){
        lblConnectionStatus.setVisible(true);
        lblConnectionStatus.setForeground(UIUtil.getLabelForeground());
        lblConnectionStatus.setText("Testing connection, please wait.");
        btnTest.setEnabled(false);
    }

    public void setConnectionStatusLabelVisible(boolean isVisible){
        lblConnectionStatus.setVisible(isVisible);
    }

    public void setTestConnectionButtonEnabled(boolean isEnabled){
        if(btnTest.isEnabled()!=isEnabled) {
            btnTest.setEnabled(isEnabled);
            btnTest.requestFocus();
        }
    }

}
