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

package com.hpe.adm.octane.ideplugins.intellij.actions.activeitem;

import com.hpe.adm.octane.ideplugins.intellij.actions.OctanePluginAction;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;
import org.json.JSONObject;

public class StopActiveItemAction extends OctanePluginAction {

    public StopActiveItemAction() {
        super("Stop work on the active item", "Stops work on active item", IconLoader.findIcon(Constants.IMG_STOP_TIMER));
    }

    @Override
    public void update(AnActionEvent e) {
        getPluginModule(e).ifPresent(pluginModule -> {
            JSONObject jsonObject = pluginModule.getInstance(IdePluginPersistentState.class).loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
            e.getPresentation().setEnabled(jsonObject != null);
        });
    }


    @Override
    public void actionPerformed(AnActionEvent e) {
        getPluginModule(e).ifPresent(pluginModule -> {
            pluginModule.getInstance(IdePluginPersistentState.class).clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
        });
    }

}