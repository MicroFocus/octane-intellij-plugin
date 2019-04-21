/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
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
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Optional;

public abstract class OctanePluginAction extends AnAction {

    public OctanePluginAction(@Nullable String text, @Nullable String description, @Nullable Icon icon){
        super(text, description, icon);
    }

    protected static Presenter getSelectedPresenter(AnActionEvent e) {
        if(e.getProject() == null) {
            return null;
        }
        PluginModule pluginModule = PluginModule.getPluginModuleForProject(e.getProject());
        TabbedPanePresenter tabbedPanePresenter = pluginModule.getInstance(TabbedPanePresenter.class);
        return tabbedPanePresenter.getSelectedPresenter();
    }

    protected static Optional<PluginModule> getPluginModule(AnActionEvent e) {
        return e.getProject() == null ? Optional.empty() : Optional.of(PluginModule.getPluginModuleForProject(e.getProject()));
    }

}