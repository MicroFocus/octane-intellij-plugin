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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions;

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailView;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;

public final class SelectFieldsAction extends AnAction {

    private boolean defaultfields = true;
    private EntityDetailView entityDetailView;

    public void setDefaultFieldsIcon(boolean defaultfields) {
        this.defaultfields = defaultfields;
    }

    public SelectFieldsAction(EntityDetailView entityDetailView) {
        super("Select fields for this entity type", "Select fields popup", IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_DEFAULT, SelectFieldsAction.class.getClassLoader()));
        this.entityDetailView = entityDetailView;
    }

    public void actionPerformed(AnActionEvent e) {
        entityDetailView.showFieldsSettings();
    }

    public void update(AnActionEvent e) {
        if (defaultfields) {
            e.getPresentation().setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_DEFAULT, SelectFieldsAction.class.getClassLoader()));
        } else {
            e.getPresentation().setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_NON_DEFAULT, SelectFieldsAction.class.getClassLoader()));
        }
    }
}
