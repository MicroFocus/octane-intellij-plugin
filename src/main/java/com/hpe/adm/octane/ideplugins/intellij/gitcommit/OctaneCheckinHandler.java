package com.hpe.adm.octane.ideplugins.intellij.gitcommit;

import com.hpe.adm.nga.sdk.model.EntityModel;
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

/**
 * Created by dulaut on 11/22/2016.
 */
public class OctaneCheckinHandler extends CheckinHandler {

    public static EntityModel activatedItem;
    private CommitMessageService commitMessageService;
    private Project project;
    private CheckinProjectPanel panel;

    public OctaneCheckinHandler(CommitMessageService commitMessageService, CheckinProjectPanel panel) {
        this.panel = panel;
        this.project = panel.getProject();
        this.commitMessageService = commitMessageService;

        panel.setCommitMessage("");
    }

    private Entity getActivatedItemType() {
        Entity type = Entity.getEntityType(activatedItem);
        if (type == Entity.DEFECT || type == Entity.USER_STORY) {
            return type;
        }
        return null;
    }

    private String getMessageForActivatedItem() {
        StringBuilder messageBuilder = new StringBuilder();
        Entity type = getActivatedItemType();
        if (type != null) {
            if (type == Entity.USER_STORY) {
                messageBuilder.append("story #");
            } else if (type == Entity.DEFECT) {
                messageBuilder.append("defect #");
            }
        } else {
            return null;
        }
        messageBuilder.append(activatedItem.getValue("id").getValue().toString() + ": ");
        messageBuilder.append(activatedItem.getValue("name").getValue().toString());
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
        SwingWorker<Boolean, Void> validateMessageWorker = new SwingWorker<Boolean, Void>() {
            @Override
            protected Boolean doInBackground() throws Exception {
                return commitMessageService.validateCommitMessage(getMessageForActivatedItem(), getActivatedItemType(),
                        Long.parseLong(activatedItem.getValue("id").getValue().toString()));
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

    private void showCommitPatterns() {
        panel.setCommitMessage("");
        SwingWorker<List<String>, Void> fetchPatternsWorker = new SwingWorker<List<String>, Void>() {
            @Override
            protected List<String> doInBackground() throws Exception {
                return commitMessageService.getCommitPatternsForStoryType(getActivatedItemType());
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
        if (activatedItem != null) {
            validate(
                    () -> panel.setCommitMessage(getMessageForActivatedItem()),
                    this::showCommitPatterns);
        }

        return super.getBeforeCheckinConfigurationPanel();
    }
}
