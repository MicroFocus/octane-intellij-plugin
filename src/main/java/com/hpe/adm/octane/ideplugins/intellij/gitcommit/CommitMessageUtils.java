package com.hpe.adm.octane.ideplugins.intellij.gitcommit;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.nga.sdk.query.Query;
import com.hpe.adm.nga.sdk.query.QueryMethod;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.nonentity.CommitMessageService;
import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.MessageType;
import com.intellij.openapi.ui.popup.Balloon;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.ui.awt.RelativePoint;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.util.*;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

public class CommitMessageUtils {

    private static final Logger logger = Logger.getInstance(CommitMessageUtils.class);

    @Inject
    private CommitMessageService commitMessageService;

    @Inject
    private Project project;

    @Inject
    private EntityService entityService;

    public void copyCommitMessageToClipboard(PartialEntity partialEntity) {

        EntityModel entityModel = PartialEntity.toEntityModel(partialEntity);
        addReferenceFieldIfNeeded(entityModel);

        String commitMessage = generateLocalCommitMessage(entityModel);

        if (!isCommitMessageValid(entityModel, commitMessage)) {

            ApplicationManager.getApplication().invokeLater(() -> showCommitPatternsWarning(entityModel));
        } else {

            StringSelection selection = new StringSelection(commitMessage);
            Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
            clipboard.setContents(selection, selection);

            ApplicationManager.getApplication().invokeLater(() ->
                    showBalloon("Commit message copied to clipboard: \"" + commitMessage + "\"", MessageType.INFO)
            );
        }
    }

    public void asyncCopyCommitMessageToClipboard(PartialEntity partialEntity) {
        Task.Backgroundable backgroundTask = new Task.Backgroundable(project, "Generating Commit Message...", true) {
            public void run(@NotNull ProgressIndicator indicator) {
                copyCommitMessageToClipboard(partialEntity);
            }
        };
        backgroundTask.queue();
    }

    public String generateLocalCommitMessage(EntityModel entityModel) {

        String taskString = "";

        if (Entity.getEntityType(entityModel) == Entity.TASK) {
            taskString = ": task #" + entityModel.getId();

            entityModel = addReferenceFieldIfNeeded(entityModel);
            entityModel = (EntityModel) entityModel.getValue("story").getValue();
        }

        StringBuilder messageBuilder = new StringBuilder();

        String id = entityModel.getId();
        Entity type = Entity.getEntityType(entityModel);

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

        messageBuilder.append(id);
        messageBuilder.append(taskString);
        return messageBuilder.toString();
    }

    public boolean isCommitMessageValid(EntityModel entityModel, String commitMessage) {

        Long id;
        Entity type;

        if (Entity.getEntityType(entityModel) == Entity.TASK) {

            entityModel = addReferenceFieldIfNeeded(entityModel);

            EntityModel taskParent = (EntityModel) entityModel.getValue("story").getValue();
            id = Long.parseLong(Objects.requireNonNull(taskParent.getId()));
            type = Entity.getEntityType(taskParent);

        } else {
            id = Long.parseLong(Objects.requireNonNull(entityModel.getId()));
            type = Entity.getEntityType(entityModel);

        }

        return commitMessageService.validateCommitMessage(commitMessage, type, id);
    }

    private EntityModel addReferenceFieldIfNeeded(EntityModel entityModel) {
        if (Entity.getEntityType(entityModel) == Entity.TASK && entityModel.getValue("story") == null) {
            EntityModel taskParent = getTaskParent(entityModel.getId());
            entityModel.setValue(new ReferenceFieldModel("story", taskParent));
        }
        return entityModel;
    }

    private EntityModel getTaskParent(String id) {
        Set<String> storyField = new HashSet<>(Collections.singletonList("story"));
        Query.QueryBuilder idQuery = Query.statement("id", QueryMethod.EqualTo, id);
        Collection<EntityModel> results = entityService.findEntities(Entity.TASK, idQuery, storyField);

        if (results.size() == 1) {
            return (EntityModel) results.iterator().next().getValue("story").getValue();
        } else {
            return null;
        }
    }

    private void showCommitPatternsWarning(EntityModel entityModel) {

        Entity type;

        if (Entity.getEntityType(entityModel) == Entity.TASK) {
            entityModel = addReferenceFieldIfNeeded(entityModel);
            type = Entity.getEntityType((EntityModel) entityModel.getValue("story"));
        } else {
            type = Entity.getEntityType(entityModel);
        }

        SwingWorker<List<String>, Void> fetchPatternsWorker = new SwingWorker<List<String>, Void>() {
            @Override
            protected List<String> doInBackground() {
                return commitMessageService.getCommitPatternsForStoryType(type);
            }

            @Override
            protected void done() {
                super.done();
                try {
                    List<String> patterns = get();

                    StringBuilder messageBuilder =
                            new StringBuilder("Cannot generate a valid commit message locally.\nMake sure it matches one of the following patterns: ");

                    String patternsString = patterns.stream()
                            .map((pattern) -> "<b>" + pattern + "</b>")
                            .collect(Collectors.joining(", "));

                    messageBuilder.append(patternsString);

                    showBalloon(messageBuilder.toString(), MessageType.WARNING);

                } catch (InterruptedException | ExecutionException ex) {
                    logger.error("Failed to fetch commit patterns from the server: " + ex);
                }
            }
        };

        fetchPatternsWorker.execute();
    }

    private void showBalloon(String message, MessageType messageType) {

        StatusBar statusBar = WindowManager.getInstance().getStatusBar(project);

        Balloon balloon = JBPopupFactory.getInstance().createHtmlTextBalloonBuilder(message,
                messageType, null)
                .setCloseButtonEnabled(true)
                .createBalloon();

        balloon.show(RelativePoint.getCenterOf(statusBar.getComponent()), Balloon.Position.atRight);
    }


}
