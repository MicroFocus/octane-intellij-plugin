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

package com.hpe.adm.octane.ideplugins.intellij.actions;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;

public class OpenMyWorkAction extends AnAction {

    public OpenMyWorkAction() {
        super("Open \"My Work\"", "Open \"My Work\"", IconLoader.findIcon(Constants.IMG_MYWORK));
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        if(e.getProject() == null) {
            return;
        }

        ToolWindow octaneToolWindow = ToolWindowManager.getInstance(e.getProject()).getToolWindow("ALM Octane");
        if (!octaneToolWindow.isActive()) {
            ToolWindowManager.getInstance(e.getProject()).getToolWindow("ALM Octane").show(null);
        }

        PluginModule pluginModule = PluginModule.getPluginModuleForProject(e.getProject());
        TabbedPanePresenter tabbedPanePresenter = pluginModule.getInstance(TabbedPanePresenter.class);
        tabbedPanePresenter.selectMyWorkTab();
    }

}