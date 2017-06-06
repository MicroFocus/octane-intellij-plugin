/*
 * Copyright 2017 Hewlett-Packard Enterprise Development Company, L.P.
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

package com.hpe.adm.octane.ideplugins.intellij.settings;

import com.hpe.adm.octane.services.connection.BasicConnectionSettingProvider;
import com.hpe.adm.octane.services.connection.ConnectionSettings;
import com.intellij.ide.passwordSafe.PasswordSafe;
import com.intellij.ide.passwordSafe.PasswordSafeException;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import org.apache.commons.lang.StringUtils;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(
        name = "OctanePluginConnectionSettings",
        presentableName = IdePersistentConnectionSettingsProvider.OctanePluginNameGetter.class
)
public class IdePersistentConnectionSettingsProvider extends BasicConnectionSettingProvider implements PersistentStateComponent<Element> {

    public static class OctanePluginNameGetter extends State.NameGetter{
        @Override
        public String get() {
            return "Octane IntelliJ Plugin";
        }
    }

    private static final String OCTANE_PASSWORD_KEY = "OCTANE_PASSWORD_KEY";

    private static final String CONNECTION_SETTINGS_TAG = "ConnectionSettings";
    private static final String URL_TAG = "Url";
    private static final String SHARED_SPACE_TAG = "SharedSpace";
    private static final String WORKSPACE_TAG = "WorkSpace";
    private static final String USER_TAG = "User";

    private Project project;

    public IdePersistentConnectionSettingsProvider(@NotNull final Project currentProject){
        this.project = currentProject;
    }

    @Nullable
    @Override
    public Element getState() {
        final Element element = new Element(CONNECTION_SETTINGS_TAG);

        if(!StringUtils.isEmpty(connectionSettings.getBaseUrl())){
            element.setAttribute(URL_TAG, String.valueOf(connectionSettings.getBaseUrl()));
        }
        if(connectionSettings.getSharedSpaceId() != null){
            element.setAttribute(SHARED_SPACE_TAG, String.valueOf(connectionSettings.getSharedSpaceId()));
        }
        if(connectionSettings.getWorkspaceId() != null){
            element.setAttribute(WORKSPACE_TAG, String.valueOf(connectionSettings.getWorkspaceId()));
        }
        if(!StringUtils.isEmpty(connectionSettings.getUserName())){
            element.setAttribute(USER_TAG, String.valueOf(connectionSettings.getUserName()));
        }

        //save the password into the password store, not related to the xml file
        if(!StringUtils.isEmpty(connectionSettings.getPassword())) {
            encryptPassword(connectionSettings.getPassword());
        }

        return element;
    }

    @Override
    public void loadState(Element state) {
        ConnectionSettings connectionSettings = new ConnectionSettings();

        if(state.getAttribute(URL_TAG) != null) {
            connectionSettings.setBaseUrl(state.getAttributeValue(URL_TAG));
        } else{
            connectionSettings.setBaseUrl("");
        }

        String sharedSpaceStr = state.getAttributeValue(SHARED_SPACE_TAG);
        if(StringUtils.isNumeric(sharedSpaceStr)){
            connectionSettings.setSharedSpaceId(Long.valueOf(sharedSpaceStr));
        }

        String workSpaceStr = state.getAttributeValue(WORKSPACE_TAG);
        if(StringUtils.isNumeric(workSpaceStr)){
            connectionSettings.setWorkspaceId(Long.valueOf(workSpaceStr));
        }

        if(state.getAttribute(USER_TAG) != null){
            connectionSettings.setUserName(state.getAttributeValue(USER_TAG));
        } else {
            connectionSettings.setUserName("");
        }

        //attempt to load password from the password store, not related to the xml file
        connectionSettings.setPassword(decryptPassword());

        setConnectionSettings(connectionSettings);
    }

    @NotNull
    private void encryptPassword(String password) {
        try {
            if((password == null || StringUtils.isBlank(password))
                    && PasswordSafe.getInstance().getPassword(project, IdePersistentConnectionSettingsProvider.class, OCTANE_PASSWORD_KEY) != null){
                PasswordSafe.getInstance().removePassword(project, IdePersistentConnectionSettingsProvider.class, OCTANE_PASSWORD_KEY);
            } else {
                PasswordSafe.getInstance().storePassword(project, IdePersistentConnectionSettingsProvider.class, OCTANE_PASSWORD_KEY, password);
            }
        } catch (NullPointerException | PasswordSafeException e) {
            //log.info("Couldn't get password for key [" + OCTANE_PASSWORD_KEY + "]", e);
        }
    }

    @NotNull
    private String decryptPassword() {
        String password;
        try {
            password = PasswordSafe.getInstance().getPassword(project, IdePersistentConnectionSettingsProvider.class, OCTANE_PASSWORD_KEY);
        } catch (NullPointerException | PasswordSafeException e) {
            //log.info("Couldn't get password for key [" + OCTANE_PASSWORD_KEY + "]", e);
            password = "";
        }
        return StringUtil.notNullize(password);
    }

}


