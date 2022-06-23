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
import org.jetbrains.annotations.Nullable;
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
                    public void setDoNotAskOption(@Nullable com.intellij.openapi.ui.DoNotAskOption doNotAsk) {
                        super.setDoNotAskOption((com.intellij.openapi.ui.DoNotAskOption) null);
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
                Writer out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f), StandardCharsets.UTF_8));
                out.append(script);
                out.flush();
                out.close();
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
