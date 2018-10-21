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
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.CommentsConversationPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.EntityFieldsPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.HTMLPresenterFXPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.HeaderPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.FieldsUtil;
import com.hpe.adm.octane.ideplugins.intellij.util.ExceptionHandler;
import com.hpe.adm.octane.ideplugins.intellij.util.RestUtil;
import com.hpe.adm.octane.ideplugins.services.CommentService;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.util.ui.JBUI;
import javafx.application.Platform;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.util.Collection;

import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.TASK;
import static javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS;

public class EntityDetailView extends JPanel implements View, Scrollable {

    private static final Logger logger = Logger.getInstance(EntityDetailView.class.getName());

    private EntityModelWrapper entityModelWrapper;
    private JBScrollPane component = new JBScrollPane(new LoadingWidget());

    private HeaderPanel headerPanel;

    private EntityFieldsPanel entityFieldsPanel;

    private CommentsConversationPanel commentsPanel;

    private HTMLPresenterFXPanel descriptionPanel;

    private SelectFieldsAction fieldsSelectAction;

    private Collection<FieldMetadata> fields;

    @Inject
    private FieldsUtil fieldsUtil;

    @Inject
    private CommentService commentService;

    @Inject
    private Project project;

    @Inject
    public EntityDetailView(HeaderPanel headerPanel, EntityFieldsPanel entityFieldsPanel, CommentsConversationPanel commentsPanel, HTMLPresenterFXPanel descriptionPanel) {

        this.headerPanel = headerPanel;
        this.entityFieldsPanel = entityFieldsPanel;
        this.commentsPanel = commentsPanel;
        this.descriptionPanel = descriptionPanel;

        //make UI
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0, 0, 0, 0};
        gridBagLayout.columnWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
        setLayout(gridBagLayout);

        Color separatorColor = UIManager.getColor("Separator.foreground");
        headerPanel.setBorder(new MatteBorder(0, 0, 1, 0, separatorColor));
        GridBagConstraints gbc_headerPanel = new GridBagConstraints();
        gbc_headerPanel.gridwidth = 2;
        gbc_headerPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_headerPanel.insets = JBUI.insets(0, 5, 5, 5);
        gbc_headerPanel.gridx = 0;
        gbc_headerPanel.gridy = 0;
        add(headerPanel, gbc_headerPanel);

        JXLabel generalLabel = new JXLabel("General");
        generalLabel.setFont(new Font(generalLabel.getFont().getName(), Font.BOLD, 18));
        GridBagConstraints gbc_GeneralTitle = new GridBagConstraints();
        gbc_GeneralTitle.anchor = GridBagConstraints.WEST;
        gbc_GeneralTitle.insets = JBUI.insets(5, 10, 10, 0);
        gbc_GeneralTitle.gridx = 0;
        gbc_GeneralTitle.gridy = 1;
        add(generalLabel, gbc_GeneralTitle);

        GridBagConstraints gbc_entityFieldsPanel = new GridBagConstraints();
        gbc_entityFieldsPanel.anchor = GridBagConstraints.NORTH;
        gbc_entityFieldsPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_entityFieldsPanel.insets = JBUI.insets(0, 10, 0, 5);
        gbc_entityFieldsPanel.gridx = 0;
        gbc_entityFieldsPanel.gridy = 2;
        gbc_entityFieldsPanel.weightx = 1.0;
        add(entityFieldsPanel, gbc_entityFieldsPanel);

        commentsPanel.setBorder(new MatteBorder(0, 1, 0, 0, separatorColor));
        GridBagConstraints gbc_commentsPanel = new GridBagConstraints();
        gbc_commentsPanel.gridheight = 3;
        gbc_commentsPanel.fill = GridBagConstraints.BOTH;
        gbc_commentsPanel.insets = JBUI.insets(0, 5);
        gbc_commentsPanel.gridx = 1;
        gbc_commentsPanel.gridy = 1;
        add(commentsPanel, gbc_commentsPanel);
        commentsPanel.setMaximumSize(new Dimension(350, getHeight()));
        commentsPanel.setMinimumSize(new Dimension(350, getHeight()));
        commentsPanel.setSize(new Dimension(350, getHeight()));
        commentsPanel.setVisible(false);

        JLabel descriptionLabel = new JLabel("Description");
        descriptionLabel.setFont(new Font(descriptionLabel.getFont().getName(), Font.BOLD, 18));
        GridBagConstraints gbc_descriptionLabel = new GridBagConstraints();
        gbc_descriptionLabel.anchor = GridBagConstraints.NORTH;
        gbc_descriptionLabel.fill = GridBagConstraints.HORIZONTAL;
        gbc_descriptionLabel.insets = JBUI.insets(10, 10, 5, 5);
        gbc_descriptionLabel.gridx = 0;
        gbc_descriptionLabel.gridy = 3;
        add(descriptionLabel, gbc_descriptionLabel);

        GridBagConstraints gbc_descriptionPanel = new GridBagConstraints();
        gbc_descriptionPanel.fill = GridBagConstraints.BOTH;
        gbc_descriptionPanel.insets = JBUI.insets(0, 5);
        gbc_descriptionPanel.gridx = 0;
        gbc_descriptionPanel.gridy = 4;
        add(descriptionPanel, gbc_descriptionPanel);

        descriptionPanel.addEventActions();

        fieldsSelectAction = new SelectFieldsAction(this);
    }

    @Override
    public JComponent getComponent() {
        component.setBorder(null);
        component.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_ALWAYS);
        component.setHorizontalScrollBarPolicy(HORIZONTAL_SCROLLBAR_NEVER);
        return component;
    }


    public void setEntityModel(EntityModelWrapper entityModelWrapper, Collection<FieldMetadata> fields) {
        this.entityModelWrapper = entityModelWrapper;

        this.fields = fields;
        // set header data
        headerPanel.setEntityModel(entityModelWrapper);
        // add comments action to header panel
        setupComments(entityModelWrapper);
        // add description data
        setupDescription(entityModelWrapper);

        // set fields data
        entityFieldsPanel.setFieldMetadata(fields);

        entityFieldsPanel.setEntityModel(entityModelWrapper, fieldsUtil.retrieveSelectedFieldsForEntity(entityModelWrapper.getEntityType()));

        fieldsSelectAction.setDefaultFieldsIcon(fieldsUtil.isDefaultState(entityModelWrapper.getEntityType()));

        component.setViewportView(this);
    }

    public void redrawFields() {
        entityFieldsPanel.setEntityModel(entityModelWrapper, fieldsUtil.retrieveSelectedFieldsForEntity(entityModelWrapper.getEntityType()));
    }

    private void setupDescription(EntityModelWrapper entityModelWrapper) {
        String descriptionContent = Util.getUiDataFromModel(entityModelWrapper.getValue(DetailsViewDefaultFields.FIELD_DESCRIPTION));
        Platform.runLater(() -> descriptionPanel.setContent(descriptionContent));
    }

    private void setupComments(EntityModelWrapper entityModelWrapper) {
        if (entityModelWrapper.getEntityType() != TASK) {
            setComments(entityModelWrapper);
            addSendNewCommentAction(entityModelWrapper);
        } else {
            headerPanel.removeCommentButton();
        }
    }

    public void setErrorMessage(String error) {
        JPanel errorPanel = new JPanel(new BorderLayout(0, 0));

        JLabel errorLabel = new JLabel();
        errorLabel.setForeground(JBColor.RED);
        errorLabel.setText("<html><center>" + error + "</center></html>");
        errorPanel.add(errorLabel);
        errorLabel.setHorizontalAlignment(SwingConstants.CENTER);
        errorLabel.setVerticalAlignment(SwingConstants.CENTER);

        component.setViewportView(errorPanel);
    }

    public void doRefresh() {
        component.setViewportView(new JBScrollPane(new LoadingWidget()));
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

            }
            catch (Exception genericException) {
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

    private void showCommentsPanel() {
        commentsPanel.setVisible(!commentsPanel.isVisible());
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

    /**
     * Methods for the Scrollable interface
     */

    @Override
    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    @Override
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
        return (getHeight() - visibleRect.height) / 10;
    }

    @Override
    public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
        return getHeight() - visibleRect.height;
    }

    @Override
    public boolean getScrollableTracksViewportWidth() {
        return false;
    }

    @Override
    public boolean getScrollableTracksViewportHeight() {
        return false;
    }

    private final class EntityCommentsAction extends AnAction {
        public EntityCommentsAction() {
            super("Show comments for current entity", "Show comments for current entity", IconLoader.findIcon(Constants.IMG_COMMENTS_ICON));
        }

        public void actionPerformed(AnActionEvent e) {
            showCommentsPanel();
        }
    }
}
