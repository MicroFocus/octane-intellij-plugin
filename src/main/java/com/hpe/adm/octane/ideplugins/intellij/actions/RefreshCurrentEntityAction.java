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

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

public class RefreshCurrentEntityAction extends OctanePluginAction {

    public RefreshCurrentEntityAction() {
        super("Refresh backlog item", "Refresh backlog item details.", IconLoader.findIcon(Constants.IMG_REFRESH_ICON, RefreshCurrentEntityAction.class.getClassLoader()));
    }

    public void actionPerformed(AnActionEvent e) {
        Presenter presenter = getSelectedPresenter(e);

        if(presenter == null) {
            return;
        }

        if(presenter instanceof EntityTreeTablePresenter) {
            ((EntityTreeTablePresenter) presenter).refresh();
        }
        else if (presenter instanceof EntityDetailPresenter) {
            ((EntityDetailPresenter) presenter).refresh();
        }

    }
}