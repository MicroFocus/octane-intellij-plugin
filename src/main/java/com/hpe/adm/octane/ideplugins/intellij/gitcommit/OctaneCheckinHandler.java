package com.hpe.adm.octane.ideplugins.intellij.gitcommit;


import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.query.Query;
import com.hpe.adm.nga.sdk.query.QueryMethod;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.services.EntityService;
import com.hpe.adm.octane.services.filtering.Entity;
import com.hpe.adm.octane.services.nonentity.CommitMessageService;
import com.hpe.adm.octane.services.util.PartialEntity;
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
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.Nullable;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.util.*;
import java.util.stream.Collectors;

public class OctaneCheckinHandler extends CheckinHandler {

    private CommitMessageService commitMessageService;
    private EntityService entityService;
    private Project project;
    private CheckinProjectPanel panel;
    private IdePluginPersistentState idePluginPersistentState;
    private PartialEntity activatedItem;
    private EntityModel parentStory;
    private String originalCommitMessage = "";

    public OctaneCheckinHandler(
            IdePluginPersistentState idePluginPersistentState,
            CommitMessageService commitMessageService,
            EntityService entityService,
            CheckinProjectPanel panel) {
        this.idePluginPersistentState = idePluginPersistentState;
        this.panel = panel;
        this.project = panel.getProject();
        this.commitMessageService = commitMessageService;
        this.entityService = entityService;
        this.originalCommitMessage = panel.getCommitMessage();
    }

    private String getMessageForActivatedItem() {

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
            messageBuilder.append(parentStory.getValue("id").getValue() + ": ");
            messageBuilder.append(parentStory.getValue("name").getValue());
            messageBuilder.append("\ntask #");
            messageBuilder.append(activatedItem.getEntityId() + ": ");
            messageBuilder.append(activatedItem.getEntityName());
        } else {
            messageBuilder.append(activatedItem.getEntityId() + ": ");
            messageBuilder.append(activatedItem.getEntityName());
        }

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

        panel.setCommitMessage("loading commit message ...");

        SwingWorker<Boolean, Void> validateMessageWorker = new SwingWorker<Boolean, Void>() {
            @Override
            protected Boolean doInBackground() throws Exception {

                if (activatedItem.getEntityType() == Entity.TASK) {
                    Set<String> storyField = new HashSet<>(Arrays.asList("story"));
                    Query.QueryBuilder idQuery = Query.statement("id", QueryMethod.EqualTo, activatedItem.getEntityId());
                    Collection<EntityModel> results = entityService.findEntities(Entity.TASK, idQuery, storyField);
                    if (!results.isEmpty()) {
                        parentStory = (EntityModel) results.iterator().next().getValue("story").getValue();
                        return commitMessageService.validateCommitMessage(
                                getMessageForActivatedItem(),
                                Entity.getEntityType(parentStory),
                                Long.parseLong(parentStory.getValue("id").getValue().toString()));
                    } else {
                        return null;
                    }
                }
                return commitMessageService.validateCommitMessage(
                        getMessageForActivatedItem(),
                        activatedItem.getEntityType(),
                        activatedItem.getEntityId());
            }

            @Override
            protected void done() {
                try {
                    if (get() == null) {
                        return;
                    } else if (get()) {
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

    private void showCommitPatterns(Entity entityType) {
        panel.setCommitMessage("");
        SwingWorker<List<String>, Void> fetchPatternsWorker = new SwingWorker<List<String>, Void>() {
            @Override
            protected List<String> doInBackground() throws Exception {
                return commitMessageService.getCommitPatternsForStoryType(entityType);
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

    private void setExpectedCommitMessage(String message) {
        String source = "{\"value\": \"" + message.replaceAll("\\n", "") + "\"}";
        JSONObject json = new JSONObject(source);
        idePluginPersistentState.saveState(IdePluginPersistentState.Key.EXPECTED_COMMIT_MESSAGE, json);
    }

    private String getExpectedCommitMessage() {
        JSONObject json = idePluginPersistentState.loadState(IdePluginPersistentState.Key.EXPECTED_COMMIT_MESSAGE);
        if (json == null)
            return null;
        return json.getString("value");
    }

    @Nullable
    @Override
    public RefreshableOnComponent getBeforeCheckinConfigurationPanel() {

        panel.getPreferredFocusedComponent().addContainerListener(new ContainerListener() {
            @Override
            public void componentAdded(ContainerEvent e) {
            }

            @Override
            public void componentRemoved(ContainerEvent e) {
                setExpectedCommitMessage(panel.getCommitMessage());
            }
        });

        activatedItem = PartialEntity.fromJsonObject(
                idePluginPersistentState.loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM));

        if (activatedItem != null) {

            validate(
                    () -> {

                        // test somehow if the commit message is generated from VCS (like when merging branches) ...
                        boolean shouldAppend = false;
                        String expectedCommitMessage = getExpectedCommitMessage();

                        if (expectedCommitMessage == null || StringUtils.isEmpty(expectedCommitMessage.trim())) {
                            // if there's no expected commit message we will append the original message if it starts with "merge"
                            if (originalCommitMessage.toLowerCase().startsWith("merge")) {
                                shouldAppend = true;
                            }
                        } else if (!originalCommitMessage.replaceAll("\\n", "").equals(expectedCommitMessage)) {
                            // if the original commmit message is not the expected one we assume it is from VCS and append it
                            shouldAppend = true;
                        }

                        if (shouldAppend)
                            panel.setCommitMessage(getMessageForActivatedItem() + "\n" + originalCommitMessage);
                        else
                            panel.setCommitMessage(getMessageForActivatedItem());
                    },
                    () -> {
                        panel.setCommitMessage(originalCommitMessage);
                        Entity type = activatedItem.getEntityType() == Entity.TASK ? Entity.getEntityType(parentStory)
                                : activatedItem.getEntityType();
                        showCommitPatterns(type);
                    }
            );
        }

        return super.getBeforeCheckinConfigurationPanel();
    }
}
