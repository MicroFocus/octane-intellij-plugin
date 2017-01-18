package com.hpe.adm.octane.ideplugins.intellij.gitcommit;

import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.PartialEntity;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.nonentity.CommitMessageService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.MessageType;
import com.intellij.openapi.ui.popup.Balloon;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.vcs.CheckinProjectPanel;
import com.intellij.openapi.vcs.checkin.CheckinHandler;
import com.intellij.openapi.vcs.ui.RefreshableOnComponent;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.ui.awt.RelativePoint;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.List;
import java.util.stream.Collectors;

public class OctaneCheckinHandler extends CheckinHandler {

    private CommitMessageService commitMessageService;
    private Project project;
    private CheckinProjectPanel panel;
    private IdePluginPersistentState idePluginPersistentState;

    public OctaneCheckinHandler(
            IdePluginPersistentState idePluginPersistentState,
            CommitMessageService commitMessageService,
            CheckinProjectPanel panel) {
        this.idePluginPersistentState = idePluginPersistentState;
        this.panel = panel;
        this.project = panel.getProject();
        this.commitMessageService = commitMessageService;

        panel.setCommitMessage("");
    }

    private String getMessageForActivatedItem(PartialEntity activatedEntity) {

        StringBuilder messageBuilder = new StringBuilder();
        Entity type = activatedEntity.getEntityType();

        if (type != null) {
            switch (type) {
                case USER_STORY:
                    messageBuilder.append("story #");
                    break;
                case QUALITY_STORY:
                    messageBuilder.append("quality story #");
                    break;
                case DEFECT:
                    messageBuilder.append("defect #");
                    break;
            }
        } else {
            return null;
        }
        messageBuilder.append(activatedEntity.getEntityId() + ": ");
        messageBuilder.append(activatedEntity.getEntityName());

        return messageBuilder.toString();
    }

    private void showWarningBalloon(List<String> commitPatterns) {

        StatusBar statusBar = WindowManager.getInstance().getStatusBar(project);

        StringBuilder messageBuilder = new StringBuilder("Please make sure your commit message " +
                "matches one of the follwing patterns: ");

        String patternsString = commitPatterns.stream()
                .map((pattern) -> "<b>" + pattern + "</b>")
                .collect(Collectors.joining(", "));
        messageBuilder.append(patternsString);

        Balloon balloon = JBPopupFactory.getInstance().createHtmlTextBalloonBuilder(messageBuilder.toString(),
                MessageType.WARNING, null)
                .setCloseButtonEnabled(true)
                .createBalloon();

        balloon.show(RelativePoint.getCenterOf(statusBar.getComponent()), Balloon.Position.atRight);
    }

    private void validate(Runnable runnableValid, Runnable runnableInvalid) {

        PartialEntity activatedItem =
                PartialEntity.fromJsonObject(
                        idePluginPersistentState.loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM));


        SwingWorker<Boolean, Void> validateMessageWorker = new SwingWorker<Boolean, Void>() {
            @Override
            protected Boolean doInBackground() throws Exception {
                return commitMessageService.validateCommitMessage(
                        getMessageForActivatedItem(activatedItem),
                        activatedItem.getEntityType(),
                        activatedItem.getEntityId());
            }

            @Override
            protected void done() {
                super.done();
                try {
                    if (get()) {
                        runnableValid.run();
                    } else {
                        runnableInvalid.run();
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };

        validateMessageWorker.execute();
    }

    private void showCommitPatterns(PartialEntity activatedItem) {
        panel.setCommitMessage("");
        SwingWorker<List<String>, Void> fetchPatternsWorker = new SwingWorker<List<String>, Void>() {
            @Override
            protected List<String> doInBackground() throws Exception {
                return commitMessageService.getCommitPatternsForStoryType(activatedItem.getEntityType());
            }

            @Override
            protected void done() {
                super.done();
                try {
                    List<String> patterns = get();
                    OctaneCheckinHandler.this.showWarningBalloon(patterns);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };

        fetchPatternsWorker.execute();
    }

    @Nullable
    @Override
    public RefreshableOnComponent getBeforeCheckinConfigurationPanel() {

        PartialEntity activatedItem =
                PartialEntity.fromJsonObject(
                        idePluginPersistentState.loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM));

        if (activatedItem != null) {
            validate(
                    () -> panel.setCommitMessage(getMessageForActivatedItem(activatedItem)), () -> showCommitPatterns(activatedItem));
        }

        return super.getBeforeCheckinConfigurationPanel();
    }
}
