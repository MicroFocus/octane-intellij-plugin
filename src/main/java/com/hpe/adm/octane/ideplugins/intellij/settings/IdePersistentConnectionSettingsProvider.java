/*******************************************************************************
 * Copyright 2017-2026 Open Text.
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

package com.hpe.adm.octane.ideplugins.intellij.settings;

import com.hpe.adm.octane.ideplugins.services.connection.BasicConnectionSettingProvider;
import com.hpe.adm.octane.ideplugins.services.connection.UserAuthentication;
import com.hpe.adm.octane.ideplugins.services.connection.granttoken.GrantTokenAuthentication;
import com.intellij.credentialStore.CredentialAttributes;
import com.intellij.credentialStore.Credentials;
import com.intellij.ide.passwordSafe.PasswordSafe;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.project.Project;
import org.apache.commons.lang3.StringUtils;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(
        name = "OctanePluginConnectionSettings",
        presentableName = IdePersistentConnectionSettingsProvider.OctanePluginNameGetter.class
)
public class IdePersistentConnectionSettingsProvider extends BasicConnectionSettingProvider implements PersistentStateComponent<Element> {

    public static class OctanePluginNameGetter extends State.NameGetter {
        @Override
        public String get() {
            return "Core Software Delivery Platform IntelliJ Plugin";
        }
    }

    private static final String CONNECTION_SETTINGS_TAG = "ConnectionSettings";
    private static final String URL_TAG = "Url";
    private static final String SHARED_SPACE_TAG = "SharedSpace";
    private static final String WORKSPACE_TAG = "WorkSpace";
    private static final String USER_TAG = "User";
    private static final String SSO_TAG = "SSO";

    private Project project;

    public IdePersistentConnectionSettingsProvider(@NotNull final Project currentProject) {
        this.project = currentProject;
    }

    @Nullable
    @Override
    public Element getState() {
        final Element element = new Element(CONNECTION_SETTINGS_TAG);

        if (!StringUtils.isEmpty(connectionSettings.getBaseUrl())) {
            element.setAttribute(URL_TAG, String.valueOf(connectionSettings.getBaseUrl()));
        }
        if (connectionSettings.getSharedSpaceId() != null) {
            element.setAttribute(SHARED_SPACE_TAG, String.valueOf(connectionSettings.getSharedSpaceId()));
        }
        if (connectionSettings.getWorkspaceId() != null) {
            element.setAttribute(WORKSPACE_TAG, String.valueOf(connectionSettings.getWorkspaceId()));
        }

        if (connectionSettings.getAuthentication() instanceof UserAuthentication) {

            UserAuthentication authentication = (UserAuthentication) connectionSettings.getAuthentication();

            if (!StringUtils.isEmpty(authentication.getAuthenticationId())) {
                element.setAttribute(USER_TAG, authentication.getAuthenticationId());
                element.setAttribute(SSO_TAG, Boolean.FALSE.toString());
            }
            //save the password into the password store, not related to the xml file
            if (!StringUtils.isEmpty(authentication.getAuthenticationSecret())) {
                encryptPassword(authentication.getAuthenticationSecret(), authentication.getAuthenticationId());
            }

        } else if (connectionSettings.getAuthentication() instanceof GrantTokenAuthentication) {
            element.setAttribute(SSO_TAG, Boolean.TRUE.toString());
        }

        return element;
    }

    @Override
    public void loadState(Element state) {

        if (state.getAttribute(URL_TAG) != null) {
            connectionSettings.setBaseUrl(state.getAttributeValue(URL_TAG));
        } else {
            connectionSettings.setBaseUrl("");
        }

        String sharedSpaceStr = state.getAttributeValue(SHARED_SPACE_TAG);
        if (StringUtils.isNumeric(sharedSpaceStr)) {
            connectionSettings.setSharedSpaceId(Long.valueOf(sharedSpaceStr));
        }

        String workSpaceStr = state.getAttributeValue(WORKSPACE_TAG);
        if (StringUtils.isNumeric(workSpaceStr)) {
            connectionSettings.setWorkspaceId(Long.valueOf(workSpaceStr));
        }

        if(state.getAttributeValue(SSO_TAG) == null || state.getAttributeValue(SSO_TAG).equals(Boolean.FALSE.toString())) {
            //user pass login
            String username = state.getAttributeValue(USER_TAG) != null ? state.getAttributeValue(USER_TAG) : "";
            String password = decryptPassword(username);
            connectionSettings.setAuthentication(new UserAuthentication(username, password));
        } else {
            connectionSettings.setAuthentication(new GrantTokenAuthentication());
        }

    }

    private void encryptPassword(String password, String userName) {
        CredentialAttributes attributes = new CredentialAttributes(
                project.getName() + project.getLocationHash(),
                userName,
                false);

        Credentials saveCredentials = new Credentials(attributes.getUserName(), password);
        PasswordSafe.getInstance().set(attributes, saveCredentials);
    }

    @NotNull
    private String decryptPassword(String username) {
        CredentialAttributes attributes = new CredentialAttributes(
                project.getName() + project.getLocationHash(),
                username,
                false);

        String pw = PasswordSafe.getInstance().getPassword(attributes);
        pw = pw == null ? "" : pw;
        return pw;
    }

}


