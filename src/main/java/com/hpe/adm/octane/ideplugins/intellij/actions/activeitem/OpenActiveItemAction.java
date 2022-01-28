/*
 * © Copyright 2017-2022 Micro Focus or one of its affiliates.
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

package com.hpe.adm.octane.ideplugins.intellij.actions.activeitem;

import com.hpe.adm.octane.ideplugins.intellij.actions.OctanePluginAction;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.granttoken.GrantTokenAuthentication;
import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import org.jetbrains.annotations.NotNull;
import org.json.JSONObject;

import javax.swing.*;
import java.util.Objects;

public class OpenActiveItemAction extends OctanePluginAction {

    private static final Icon defaultActiveIcon = IconLoader.findIcon(Constants.IMG_OCTANE_LOGO_20x20);

    public OpenActiveItemAction() {
        super("Open active backlog item", "Open a detail tab with the active backlog item.", defaultActiveIcon);
    }

    @Override
    public void update(@NotNull AnActionEvent e) {
        if (e.getProject() == null) {
            return;
        }

        getPluginModule(e).ifPresent(pluginModule -> {

            JSONObject jsonObject = pluginModule
                    .getInstance(IdePluginPersistentState.class)
                    .loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);

            boolean isEnabled = jsonObject != null && !pluginModule.isSsoLoginInProgress;
            e.getPresentation().setEnabled(isEnabled);

            if (jsonObject != null) {
                PartialEntity activeItem = PartialEntity.fromJsonObject(jsonObject);

                JSONObject prevJsonObject = pluginModule.getInstance(IdePluginPersistentState.class).loadState(IdePluginPersistentState.Key.PREV_ACTIVE_WORK_ITEM);
                PartialEntity prevActiveItem = PartialEntity.fromJsonObject(prevJsonObject);

                String activeItemId = "#" + activeItem.getEntityId();

                // This is necessary to avoid doing a rest call for labels on init
                if (!Objects.equals(activeItem, prevActiveItem) || !Objects.equals(e.getPresentation().getText(), activeItemId)) {
                    // Set the new state for previous active item
                    pluginModule.getInstance(IdePluginPersistentState.class).saveState(IdePluginPersistentState.Key.PREV_ACTIVE_WORK_ITEM, jsonObject);

                    e.getPresentation().setDescription(activeItem.getEntityName());
                    e.getPresentation().setText(activeItemId);

                    ConnectionSettings connectionSettings =
                            e.getProject()
                                    .getService(IdePersistentConnectionSettingsProvider.class)
                                    .getConnectionSettings();

                    // SSO login does not have dynamic icon in order to not trigger grant token login
                    if (connectionSettings.getAuthentication() instanceof GrantTokenAuthentication) {
                        e.getPresentation().setIcon(defaultActiveIcon);
                    } else {
                        pluginModule.getInstance(EntityIconFactory.class).getIconAsImageAsync(
                                activeItem.getEntityType(),
                                20,
                                11,
                                image -> {
                                    ImageIcon imageIcon = new ImageIcon(image);
                                    e.getPresentation().setIcon(imageIcon);
                                }
                        );
                    }
                }
            } else {
                pluginModule.getInstance(EntityIconFactory.class).getIconAsImageAsync(
                        null,
                        20,
                        11,
                        image -> {
                            ImageIcon imageIcon = new ImageIcon(image);
                            e.getPresentation().setIcon(imageIcon);
                            e.getPresentation().setText("No Active Item");
                        }
                );
            }
        });
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        if (e.getProject() == null) {
            return;
        }

        getPluginModule(e).ifPresent(pluginModule -> {
            JSONObject jsonObject = pluginModule.getInstance(IdePluginPersistentState.class).loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
            if (jsonObject != null) {
                ToolWindow octaneToolWindow = ToolWindowManager.getInstance(Objects.requireNonNull(e.getProject())).getToolWindow("ALM Octane");
                if (!octaneToolWindow.isActive()) {
                    ToolWindowManager.getInstance(e.getProject()).getToolWindow("ALM Octane").show(null);
                }
                TabbedPanePresenter tabbedPanePresenter = pluginModule.getInstance(TabbedPanePresenter.class);
                tabbedPanePresenter.openDetailTab(PartialEntity.fromJsonObject(jsonObject));
            }
        });
    }

}