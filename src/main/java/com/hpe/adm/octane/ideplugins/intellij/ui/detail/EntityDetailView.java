/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.LoadingWidget;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions.SelectFieldsAction;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.CommentsPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.EntityFieldsPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.HeaderPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.FieldsUtil;
import com.hpe.adm.octane.ideplugins.intellij.util.ExceptionHandler;
import com.hpe.adm.octane.ideplugins.intellij.util.RestUtil;
import com.hpe.adm.octane.ideplugins.services.CommentService;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;

import javax.swing.*;
import java.awt.*;
import java.util.Collection;

import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.TASK;

public class EntityDetailView extends JPanel implements View {

    private static final Logger logger = Logger.getInstance(EntityDetailView.class.getName());

    private HeaderPanel headerPanel;
    private EntityFieldsPanel entityFieldsPanel;
    private CommentsPanel commentsPanel;
    private SelectFieldsAction fieldsSelectAction;
    private JPanel entityPanel;
    private JSplitPane splitPane;
    private boolean isCommentsPanelVisible;

    private EntityModelWrapper entityModelWrapper;
    private Collection<FieldMetadata> fields;

    @Inject
    private FieldsUtil fieldsUtil;

    @Inject
    private CommentService commentService;

    @Inject
    private Project project;

    static {
        UIManager.getLookAndFeelDefaults().put("SplitPane.darkShadow", JBColor.border());
    }

    @Inject
    public EntityDetailView(HeaderPanel headerPanel, EntityFieldsPanel entityFieldsPanel, CommentsPanel commentsPanel) {

        this.headerPanel = headerPanel;
        this.entityFieldsPanel = entityFieldsPanel;
        this.commentsPanel = commentsPanel;

        setLayout(new BorderLayout());
        showLoading();

        entityPanel = new JPanel(new BorderLayout());
        entityPanel.add(headerPanel, BorderLayout.NORTH);

        JPanel fieldsScrollPaneWrapper = new JPanel(new BorderLayout());
        fieldsScrollPaneWrapper.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        JBScrollPane fieldsScrollPane = new JBScrollPane(entityFieldsPanel);
        fieldsScrollPaneWrapper.add(fieldsScrollPane);

        commentsPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, fieldsScrollPaneWrapper, commentsPanel);
        splitPane.setOneTouchExpandable(true);
        splitPane.setContinuousLayout(true);
        splitPane.setResizeWeight(1);
        splitPane.setBorder(null);
        entityPanel.add(splitPane, BorderLayout.CENTER);
        showCommentsPanel(false);

        fieldsSelectAction = new SelectFieldsAction(this);
    }

    @Override
    public JComponent getComponent() {
        return this;
    }

    private void showLoading(){
        removeAll();
        add(new LoadingWidget(), BorderLayout.CENTER);
    }

    private void showEntity(){
        removeAll();
        add(entityPanel, BorderLayout.CENTER);
    }

    private void showError(Component component){
        removeAll();
        add(component, BorderLayout.CENTER);
    }


    public void setEntityModel(EntityModelWrapper entityModelWrapper, Collection<FieldMetadata> fields) {
        this.entityModelWrapper = entityModelWrapper;

        this.fields = fields;
        // set header data
        headerPanel.setEntityModel(entityModelWrapper);
        // add comments action to header panel
        setupComments(entityModelWrapper);

        // set fields data
        entityFieldsPanel.setFieldMetadata(fields);

        entityFieldsPanel.setEntityModel(entityModelWrapper, fieldsUtil.retrieveSelectedFieldsForEntity(entityModelWrapper.getEntityType()));

        fieldsSelectAction.setDefaultFieldsIcon(fieldsUtil.isDefaultState(entityModelWrapper.getEntityType()));

        showEntity();
    }

    public void redrawFields() {
        entityFieldsPanel.setEntityModel(entityModelWrapper, fieldsUtil.retrieveSelectedFieldsForEntity(entityModelWrapper.getEntityType()));
    }

    private void setupComments(EntityModelWrapper entityModelWrapper) {
        setComments(entityModelWrapper);
        addSendNewCommentAction(entityModelWrapper);
    }

    public void setErrorMessage(String error) {
        JPanel errorPanel = new JPanel(new BorderLayout(0, 0));

        JLabel errorLabel = new JLabel();
        errorLabel.setForeground(JBColor.RED);
        errorLabel.setText("<html><center>" + error + "</center></html>");
        errorPanel.add(errorLabel);
        errorLabel.setHorizontalAlignment(SwingConstants.CENTER);
        errorLabel.setVerticalAlignment(SwingConstants.CENTER);

        showError(errorLabel);
    }

    public void doRefresh() {
        showLoading();
    }

    public void setRefreshEntityButton(AnAction refreshButton) {
        headerPanel.setRefreshButton(refreshButton);
    }


    public void setSaveSelectedPhaseButton(AnAction saveSelectedPhaseAction) {
        headerPanel.setSaveButton(saveSelectedPhaseAction);
    }

    /**
     * Comment action related functions
     */

    public void setupCommentsButton() {
        headerPanel.setCommentButton(new EntityCommentsAction());
    }

    private String getCommentMessageBoxText() {
        return commentsPanel.getCommentMessageBoxText();
    }

    private void addSendNewCommentAction(EntityModelWrapper entityModelWrapper) {
        commentsPanel.addSendNewCommentAction(e -> {
            try {
                commentService.postComment(entityModelWrapper.getEntityModel(), getCommentMessageBoxText());

            } catch (OctaneException octaneException) {
                ExceptionHandler exceptionHandler = new ExceptionHandler(octaneException, project);
                exceptionHandler.showErrorNotification();
                logger.error(octaneException);

            } catch (Exception genericException) {
                ExceptionHandler exceptionHandler = new ExceptionHandler(genericException, project);
                exceptionHandler.showErrorNotification();

                logger.error(genericException);
            }
            commentsPanel.setCommentMessageBoxText("");
            setComments(entityModelWrapper);
        });
    }

    private void setComments(EntityModelWrapper entityModelWrapper) {
        RestUtil.runInBackground(() -> commentService.getComments(entityModelWrapper.getEntityModel()), (comments) -> commentsPanel.setComments(comments), null, "Failed to get possible comments", "fetching comments");
    }

    private void showCommentsPanel(boolean shouldShow) {
        if(shouldShow) {
            splitPane.setDividerLocation(0.8);
            commentsPanel.setVisible(true);
            splitPane.setDividerSize(10);
        } else {
            splitPane.setDividerLocation(1.0);
            commentsPanel.setVisible(false);
            splitPane.setDividerSize(0);
        }
        isCommentsPanelVisible = shouldShow;
    }

    public void setOpenInBrowserButton() {
        headerPanel.setOpenInBrowserButton();
    }

    /**
     * Field popup related functions
     */

    public void setupFieldsSelectButton() {
        setupFieldsSelectButton(fieldsSelectAction);
    }

    private void setupFieldsSelectButton(SelectFieldsAction fieldSelectButton) {
        headerPanel.setFieldSelectButton(fieldSelectButton);
    }

    public void showFieldsSettings() {
        FieldsSelectPopup fieldsPopup = PluginModule.getPluginModuleForProject(project).getInstance(FieldsSelectPopup.class);

        fieldsPopup.setEntityDetails(entityModelWrapper, fields, fieldsSelectAction);

        fieldsPopup.setLocation(headerPanel.getFieldsPopupLocation().x - (int) fieldsPopup.getPreferredSize().getWidth(),
                headerPanel.getFieldsPopupLocation().y);

        fieldsPopup.setVisible(!fieldsPopup.isVisible());
    }

    private final class EntityCommentsAction extends AnAction {
        public EntityCommentsAction() {
            super("Show comments for current entity", "Show comments for current entity", IconLoader.findIcon(Constants.IMG_COMMENTS_ICON));
        }

        public void actionPerformed(AnActionEvent e) {
            showCommentsPanel(!isCommentsPanelVisible);
        }
    }
}
