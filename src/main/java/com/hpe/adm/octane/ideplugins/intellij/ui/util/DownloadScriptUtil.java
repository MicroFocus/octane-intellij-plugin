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

package com.hpe.adm.octane.ideplugins.intellij.ui.util;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.util.RestUtil;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.nonentity.DownloadScriptService;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ide.actions.OpenProjectFileChooserDescriptor;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.fileChooser.FileChooser;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vcs.VcsShowConfirmationOption;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ui.ConfirmationDialog;
import org.jsoup.Jsoup;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

public class DownloadScriptUtil {
    @Inject
    private DownloadScriptService scriptService;

    @Inject
    private EntityService entityService;

    @Inject
    private Project project;

    public void downloadScriptForTest(EntityModel test) {
        VirtualFile selectedFolder = chooseScriptFolder(project);
        if (selectedFolder != null) {
            long testId = Long.parseLong(test.getValue("id").getValue().toString());
            String testName, scriptFileName;

            if(Entity.getEntityType(test) == Entity.BDD_SCENARIO) {
                EntityModel bddScenario = entityService.findEntity(Entity.BDD_SCENARIO, testId, Collections.singleton("bdd_spec"));
                testName = Util.getUiDataFromModel(bddScenario.getValue("bdd_spec"));
                String bddSpecId = Util.getUiDataFromModel(bddScenario.getValue("bdd_spec"), "id");
                scriptFileName = testName + "_" + bddSpecId + ".feature";
            } else {
                testName = test.getValue("name").getValue().toString();
                testName = removeHtmlTags(testName);
                scriptFileName = testName + "_" + testId + ".feature";
            }

            boolean shouldDownloadScript = true;
            if (selectedFolder.findChild(scriptFileName) != null) {
                String title = "Confirm file overwrite";
                String message = "Selected destination folder already contains a file named \"" +
                        scriptFileName + "\". Do you want to overwrite this file?";

                ConfirmationDialog dialog = new ConfirmationDialog(project, message, title,
                        null, VcsShowConfirmationOption.STATIC_SHOW_CONFIRMATION) {
                    @Override
                    protected boolean canBeHidden() {
                        return false;
                    }
                };
                shouldDownloadScript = dialog.showAndGet();
            }

            if (shouldDownloadScript) {
                RestUtil.LOADING_MESSAGE = "Downloading script for " + test.getType() + " test with id " + testId;
                RestUtil.runInBackground(
                        () -> {
                            String scriptContent;
                            try {
                                scriptContent = scriptService.getTestScriptContent(testId);
                            } catch (UnsupportedEncodingException e) {
                                scriptContent = null;
                                UiUtil.showWarningBalloon(project,
                                        "Unsupported Encoding",
                                        "The script you are trying to download contains unsupported characters.",
                                        NotificationType.WARNING);
                            }
                            return createTestScriptFile(selectedFolder.getPath(), scriptFileName, scriptContent);
                        },
                        (scriptFile) -> {
                            VirtualFile vFile = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(scriptFile);
                            FileEditorManager.getInstance(project).openFile(vFile, true, true);
                            // getProjectFile() returns .../{project_directory}/.idea/misc.xml, and we need to remove /.idea/misc.xml
                            project.getProjectFile().getParent().getParent().refresh(false, true);
                        },
                        project,
                        "failed to download script for " + test.getType() + " test with id " + testId);
            }
        }
    }

    private VirtualFile chooseScriptFolder(Project project) {
        FileChooserDescriptor descriptor = new OpenProjectFileChooserDescriptor(true);
        descriptor.setHideIgnored(false);
        // getProjectFile() returns .../{project_directory}/.idea/misc.xml, and we need to remove /.idea/misc.xml
        descriptor.setRoots(project.getProjectFile().getParent().getParent());
        descriptor.setTitle("Select Parent Folder");
        descriptor.withTreeRootVisible(false);
        VirtualFile[] virtualFile = FileChooser.chooseFiles(descriptor, null, null);

        return (virtualFile.length != 1) ? null : virtualFile[0];
    }

    private File createTestScriptFile(String path, String fileName, String script) {
        File f = new File(path + "/" + fileName.replaceAll("[\\\\/:?*\"<>|]", ""));
        try {
            f.createNewFile();
            if (script != null) {
                try (Writer out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f), StandardCharsets.UTF_8))) {
                    out.append(script);
                }
            }
        } catch (IOException e) {
            return null;
        }
        return f;
    }

    private String removeHtmlTags(String testName) {
        return Jsoup.parse(testName).text();
    }
}
