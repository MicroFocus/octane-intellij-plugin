package com.hpe.adm.octane.ideplugins.intellij;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.services.ConnectionSettings;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.components.StoragePathMacros;
import com.intellij.openapi.diagnostic.Logger;
import org.jdom.Element;
import org.jetbrains.annotations.Nullable;

@State(
        name = "ConnectionSettings",
        storages = {
                @Storage(
                        file = StoragePathMacros.APP_CONFIG + "/octane_connection_settings.xml"
                )}
)
public class ConnectionSettingsWrapper implements PersistentStateComponent<Element> {
    private static final String CONNECTION_SETTINGS_TAG = "ConnectionSettings";
    private static final String URL_TAG = "Url";
    private static final String SHARED_SPACE_TAG = "SharedSpace";
    private static final String WORKSPACE_TAG = "WorkSpace";
    private static final String USER_TAG = "User";
    private static final String PASSWORD_TAG = "Password";
    private Logger pluginLogger;
    @Inject
    private ConnectionSettings connectionSettings;

    @Nullable
    @Override
    public Element getState() {
        final Element element = new Element(CONNECTION_SETTINGS_TAG);

        element.setAttribute(URL_TAG, connectionSettings.getBaseUrl());
        element.setAttribute(SHARED_SPACE_TAG, String.valueOf(connectionSettings.getSharedSpaceId()));
        element.setAttribute(WORKSPACE_TAG, String.valueOf(connectionSettings.getWorkspaceId()));
        element.setAttribute(USER_TAG, connectionSettings.getUserName());
        element.setAttribute(PASSWORD_TAG, connectionSettings.getPassword());

        return element;
    }

    @Override
    public void loadState(Element state) {
        try {
            connectionSettings.setBaseUrl(state.getAttributeValue(URL_TAG));
            connectionSettings.setSharedSpaceId(Long.valueOf(state.getAttributeValue(SHARED_SPACE_TAG)));
            connectionSettings.setWorkspaceId(Long.valueOf(state.getAttributeValue(WORKSPACE_TAG)));
            connectionSettings.setUserName(state.getAttributeValue(USER_TAG));
            connectionSettings.setPassword(state.getAttributeValue(PASSWORD_TAG));

        } catch (Exception e) {
            pluginLogger.error("Error while trying to load the connection settings");
        }
    }

    public void setLog(Logger log) {
        this.pluginLogger = log;
    }
}


