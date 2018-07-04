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
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.LoadingWidget;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions.SelectFieldsAction;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.CommentsConversationPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.EntityFieldsPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.HTMLPresenterFXPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.HeaderPanel;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.DefaultEntityFieldsUtil;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.ui.components.JBScrollPane;
import javafx.application.Platform;
import org.json.JSONObject;

import javax.swing.*;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS;

public class EntityDetailView extends JPanel implements View, Scrollable {

    private EntityModel entityModel;
    private JBScrollPane component = new JBScrollPane(new LoadingWidget());


    private HeaderPanel headerPanel;


    private EntityFieldsPanel entityFieldsPanel;


    private CommentsConversationPanel commentsPanel;


    private HTMLPresenterFXPanel descriptionPanel;

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private FieldsSelectFrame fieldsPopup;

    private Collection<FieldMetadata> fields;
    private Map<Entity, Set<String>> defaultFields;
    private Map<Entity, Set<String>> selectedFields;

    private FieldsSelectFrame.SelectionListener selectionListener;

    @Inject
    public EntityDetailView(HeaderPanel headerPanel, EntityFieldsPanel entityFieldsPanel, CommentsConversationPanel commentsPanel, HTMLPresenterFXPanel descriptionPanel) {
        defaultFields = DefaultEntityFieldsUtil.getDefaultFields();

        this.headerPanel = headerPanel;
        this.entityFieldsPanel = entityFieldsPanel;
        this.commentsPanel = commentsPanel;
        this.descriptionPanel = descriptionPanel;

        //make UI
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0, 0, 0, 0};
        gridBagLayout.columnWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
        setLayout(gridBagLayout);

        headerPanel.setBorder(new MatteBorder(0, 0, 1, 0, new Color(0, 0, 0)));
        GridBagConstraints gbc_headerPanel = new GridBagConstraints();
        gbc_headerPanel.gridwidth = 2;
        gbc_headerPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_headerPanel.insets = new Insets(5, 0, 5, 5);
        gbc_headerPanel.gridx = 0;
        gbc_headerPanel.gridy = 0;
        add(headerPanel, gbc_headerPanel);

        GridBagConstraints gbc_entityFieldsPanel = new GridBagConstraints();
        gbc_entityFieldsPanel.anchor = GridBagConstraints.NORTH;
        gbc_entityFieldsPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_entityFieldsPanel.insets = new Insets(0, 5, 0, 5);
        gbc_entityFieldsPanel.gridx = 0;
        gbc_entityFieldsPanel.gridy = 1;
        add(entityFieldsPanel, gbc_entityFieldsPanel);

        commentsPanel.setBorder(new MatteBorder(0, 1, 0, 0, new Color(0, 0, 0)));
        GridBagConstraints gbc_commentsPanel = new GridBagConstraints();
        gbc_commentsPanel.gridheight = 3;
        gbc_commentsPanel.fill = GridBagConstraints.BOTH;
        gbc_commentsPanel.insets = new Insets(0, 5, 0, 5);
        gbc_commentsPanel.gridx = 1;
        gbc_commentsPanel.gridy = 1;
        add(commentsPanel, gbc_commentsPanel);
        commentsPanel.setMaximumSize(new Dimension(350, getHeight()));
        commentsPanel.setMinimumSize(new Dimension(350, getHeight()));
        commentsPanel.setSize(new Dimension(350, getHeight()));
        commentsPanel.setVisible(false);

        JLabel descriptionLabel = new JLabel("Description");
        descriptionLabel.setFont(new Font("Arial", Font.BOLD, 18));
        GridBagConstraints gbc_descriptionLabel = new GridBagConstraints();
        gbc_descriptionLabel.anchor = GridBagConstraints.NORTH;
        gbc_descriptionLabel.fill = GridBagConstraints.HORIZONTAL;
        gbc_descriptionLabel.insets = new Insets(10, 12, 5, 5);
        gbc_descriptionLabel.gridx = 0;
        gbc_descriptionLabel.gridy = 2;
        add(descriptionLabel, gbc_descriptionLabel);

        GridBagConstraints gbc_descriptionPanel = new GridBagConstraints();
        gbc_descriptionPanel.fill = GridBagConstraints.BOTH;
        gbc_descriptionPanel.insets = new Insets(0, 5, 0, 5);
        gbc_descriptionPanel.gridx = 0;
        gbc_descriptionPanel.gridy = 3;
        add(descriptionPanel, gbc_descriptionPanel);

        descriptionPanel.addEventActions();
    }

    @Override
    public JComponent getComponent() {
        component.setBorder(null);
        component.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_ALWAYS);
        component.setHorizontalScrollBarPolicy(HORIZONTAL_SCROLLBAR_NEVER);
        component.setMinimumSize(new Dimension(0, 0));
        return component;
    }


    public void setEntityModel(EntityModel entityModel, Collection<FieldMetadata> fields) {
        this.entityModel = entityModel;
        this.fields = fields;
        // set selected fields
        retrieveSelectedFieldsFromPersistentState();
        // set header data
        headerPanel.setEntityModel(entityModel);
        // set description data
        final String descriptionContent = Util.getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DESCRIPTION));
        Platform.runLater(() -> descriptionPanel.setContent(descriptionContent));
        // set fields data
        entityFieldsPanel.setFields(fields);
        entityFieldsPanel.setEntityModel(entityModel, selectedFields.get(Entity.getEntityType(entityModel)));
        component.setViewportView(this);
    }

    public EntityModel getEntityModel() {
        return this.entityModel;
    }

    public void setErrorMessage(String error) {
        JPanel errorPanel = new JPanel(new BorderLayout(0, 0));

        JLabel errorLabel = new JLabel();
        errorLabel.setForeground(Color.RED);
        errorLabel.setText("<html><center>" + error + "</center></html>");
        errorPanel.add(errorLabel);
        errorLabel.setHorizontalAlignment(SwingConstants.CENTER);
        errorLabel.setVerticalAlignment(SwingConstants.CENTER);

        component.setViewportView(errorPanel);
    }

    public void doRefresh() {
        component.setViewportView(new JBScrollPane(new LoadingWidget()));
    }

    public void addFieldSelectListener(FieldsSelectFrame.SelectionListener selectionListener) {
        this.selectionListener = selectionListener;
    }

    public void retrieveSelectedFieldsFromPersistentState() {
        JSONObject selectedFieldsJson = idePluginPersistentState.loadState(IdePluginPersistentState.Key.SELECTED_FIELDS);
        if (selectedFieldsJson == null) {
            selectedFields = defaultFields;
        } else {
            selectedFields = DefaultEntityFieldsUtil.entityFieldsFromJson(selectedFieldsJson.toString());
            if (selectedFields.get(Entity.getEntityType(entityModel)) == null) {
                selectedFields = defaultFields;
            }
        }
    }

    public void setRefreshEntityButton(AnAction refreshButton) {
        headerPanel.setRefreshButton(refreshButton);
    }

    public void setCommentsEntityButton(AnAction commentsButton) {
        headerPanel.setCommentButton(commentsButton);
    }

    public void setSaveSelectedPhaseButton(AnAction saveSelectedPhaseAction) {
        headerPanel.setSaveButton(saveSelectedPhaseAction);
    }

    public void setOpenInBrowserButton(AnAction openInBrowserAction) {
        headerPanel.setOpenInBrowserButton(openInBrowserAction);
    }

    public void setPhaseInHeader(boolean showPhase) {
        headerPanel.setPhaseInHeader(showPhase);
    }

    public FieldModel getSelectedTransition() {
        return headerPanel.getSelectedTransition();
    }

    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
        headerPanel.setPossiblePhasesForEntity(phasesList);
    }

    public void setComments(Collection<EntityModel> comments) {
        commentsPanel.clearCurrentComments();
        for (EntityModel comment : comments) {
            String commentsPostTime = Util.getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_CREATION_TIME));
            String userName = Util.getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_AUTHOR), "full_name");
            String commentLine = Util.getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_COMMENT_TEXT));
            commentsPanel.addExistingComment(commentsPostTime, userName, commentLine);
        }
        commentsPanel.setChatBoxScene();
    }

    public void addSendNewCommentAction(ActionListener actionListener) {
        commentsPanel.addSendNewCommentAction(actionListener);
    }

    public void showFieldsSettings() {
        fieldsPopup.setLocation(headerPanel.getFieldsPopupLocation().x - (int) fieldsPopup.getPreferredSize().getWidth(),
                headerPanel.getFieldsPopupLocation().y);
        fieldsPopup.setVisible(!fieldsPopup.isVisible());
    }

    public void setCommentMessageBoxText(String t) {
        commentsPanel.setCommentMessageBoxText(t);
    }

    public String getCommentMessageBoxText() {
        return commentsPanel.getCommentMessageBoxText();
    }

    public void showCommentsPanel() {
        commentsPanel.setVisible(!commentsPanel.isVisible());
    }

    public void setFieldSelectButton(SelectFieldsAction fieldSelectButton) {
        headerPanel.setFieldSelectButton(fieldSelectButton);
        fieldsPopup = new FieldsSelectFrame(defaultFields.get(Entity.getEntityType(entityModel)),
                fields.stream()
                        .filter(e -> !Arrays.asList("phase", "name", "subtype", "description").contains(e.getName()))
                        .collect(Collectors.toList()),
                selectedFields,
                Entity.getEntityType(entityModel),
                idePluginPersistentState,
                fieldSelectButton);
        fieldsPopup.addSelectionListener(e -> entityFieldsPanel.setEntityModel(entityModel, fieldsPopup.getSelectedFields()));
        fieldsPopup.addSelectionListener(selectionListener);
    }


    public Set<String> getSelectedFields() {
        return fieldsPopup.getSelectedFields();
    }

    public void setSelectedFields(Set<String> selectedFields) {
        fieldsPopup.setSelectedFieldsFromOtherTab(selectedFields);
    }

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
}
