/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors (“Open Text”) are as may be set forth
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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions;

import com.intellij.execution.RunManager;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.Presentation;
import com.intellij.openapi.project.Project;


public class PhaseItemAction extends AnAction {
    private static final int SUMMARY_LENGHT = 10;
    private final RunnerAndConfigurationSettings myConfiguration;
    private final Project myProject;
    private String phaseName = " ";

    public PhaseItemAction(final RunnerAndConfigurationSettings configuration, final Project project) {
        this.myConfiguration = configuration;
        this.myProject = project;
        this.phaseName = configuration.getName();

        if (phaseName == null || phaseName.length() == 0) {
            phaseName = " ";
        }

        if (phaseName.length() > SUMMARY_LENGHT) {
            phaseName = phaseName.substring(0, SUMMARY_LENGHT) + "...";
        }
        final Presentation presentation = getTemplatePresentation();
        presentation.setText(phaseName, false);
        updateIcon(presentation);
    }

    private void updateIcon(final Presentation presentation) {
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        RunManager.getInstance(myProject).setSelectedConfiguration(myConfiguration);
//        updatePresentation(ExecutionTargetManager.getActiveTarget(myProject), myConfiguration, myProject, e.getPresentation());
    }

    @Override
    public void update(final AnActionEvent e) {
        super.update(e);
        updateIcon(e.getPresentation());
    }

}
