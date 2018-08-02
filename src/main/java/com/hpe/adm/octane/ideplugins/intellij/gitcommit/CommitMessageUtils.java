package com.hpe.adm.octane.ideplugins.intellij.gitcommit;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.query.Query;
import com.hpe.adm.nga.sdk.query.QueryMethod;
import com.hpe.adm.octane.ideplugins.intellij.util.ExceptionHandler;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.nonentity.CommitMessageService;
import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.MessageType;
import com.intellij.openapi.ui.popup.Balloon;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.ui.awt.RelativePoint;

import javax.swing.*;
import java.util.*;
import java.util.stream.Collectors;

public class CommitMessageUtils {

    private PartialEntity activatedItem;
    private EntityModel parentStory;

    @Inject
    private CommitMessageService commitMessageService;

    @Inject
    private Project project;

    @Inject
    private EntityService entityService;

    public CommitMessageUtils() {

    }

    public String getCommitMessage(PartialEntity activatedItem) {

        if (validate(activatedItem)) {
            return getMessageForActivatedItem(activatedItem);
        } else {
            ExceptionHandler exceptionHandler = new ExceptionHandler(new Exception("Failed to validate commit message"), project);
            return "";
        }
    }

    private String getMessageForActivatedItem(PartialEntity activatedItem) {

        StringBuilder messageBuilder = new StringBuilder();
        Entity type = activatedItem.getEntityType() == Entity.TASK ? Entity.getEntityType(parentStory)
                : activatedItem.getEntityType();

        if (type != null) {
            switch (type) {
                case USER_STORY:
                    messageBuilder.append("user story #");
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
        if (activatedItem.getEntityType() == Entity.TASK) {
            messageBuilder.append(parentStory.getValue("id").getValue());
            messageBuilder.append(": task #");
        }
        messageBuilder.append(activatedItem.getEntityId() + ": ");

        return messageBuilder.toString();
    }

    private boolean validate(PartialEntity activatedItem) {
        if (activatedItem.getEntityType() == Entity.TASK) {
            Set<String> storyField = new HashSet<>(Arrays.asList("story"));
            Query.QueryBuilder idQuery = Query.statement("id", QueryMethod.EqualTo, activatedItem.getEntityId());
            Collection<EntityModel> results = entityService.findEntities(Entity.TASK, idQuery, storyField);
            if (!results.isEmpty()) {
                parentStory = (EntityModel) results.iterator().next().getValue("story").getValue();
                return commitMessageService.validateCommitMessage(
                        getMessageForActivatedItem(activatedItem),
                        Entity.getEntityType(parentStory),
                        Long.parseLong(parentStory.getValue("id").getValue().toString()));
            } else {
                return false;
            }
        }
        return commitMessageService.validateCommitMessage(
                getMessageForActivatedItem(activatedItem),
                activatedItem.getEntityType(),
                activatedItem.getEntityId());

    }

    public void showCommitPatterns(PartialEntity activatedItem) {
        Entity type = activatedItem.getEntityType() == Entity.TASK ? Entity.getEntityType(parentStory)
                : activatedItem.getEntityType();
        SwingWorker<List<String>, Void> fetchPatternsWorker = new SwingWorker<List<String>, Void>() {
            @Override
            protected List<String> doInBackground() throws Exception {
                return commitMessageService.getCommitPatternsForStoryType(type);
            }

            @Override
            protected void done() {
                super.done();
                try {
                    List<String> patterns = get();
                    showWarningBalloon(patterns);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };

        fetchPatternsWorker.execute();
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


}
