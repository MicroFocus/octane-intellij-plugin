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

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.Scrollable;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;

import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.json.JSONObject;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions.SelectFieldsAction;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.CommentsConversationPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.EntityFieldsPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.HTMLPresenterFXPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.HeaderPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.DefaultEntityFieldsUtil;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.DataKeys;
import com.intellij.openapi.project.Project;
import com.intellij.ui.JBColor;

import javafx.application.Platform;

public class GeneralEntityDetailsPanel extends JPanel implements Scrollable {

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private Map<Entity, Set<String>> defaultFields = DefaultEntityFieldsUtil.getDefaultFields();
    private Map<Entity, Set<String>> selectedFields;
    private JXPanel commentsDetails;
    private FieldsSelectFrame fieldsPopup;
    private HTMLPresenterFXPanel descriptionDetails;
    private EntityModel entityModel;
    private Collection<FieldMetadata> fields;
    private HeaderPanel headerPanel;
    private CommentsConversationPanel commentsListPanel;
    private JXLabel label;
    private String baseUrl;

    private EntityFieldsPanel entityFieldsPanel;

    private FieldsSelectFrame.SelectionListener selectionListener;

    public GeneralEntityDetailsPanel(EntityModel entityModel, Collection<FieldMetadata> fields) {
        setLayout(new BorderLayout(0, 0));

        this.entityModel = entityModel;
        this.fields = fields;

        DataContext dataContext = DataManager.getInstance().getDataContextFromFocus().getResult();
        Project project = DataKeys.PROJECT.getData(dataContext);
        PluginModule module = PluginModule.getPluginModuleForProject(project);
        baseUrl = module.getInstance(ConnectionSettingsProvider.class).getConnectionSettings().getBaseUrl();

        idePluginPersistentState = module.getInstance(IdePluginPersistentState.class);

        JSONObject selectedFieldsJson = idePluginPersistentState.loadState(IdePluginPersistentState.Key.SELECTED_FIELDS);
        if (selectedFieldsJson == null) {
            selectedFields = defaultFields;
        } else {
            selectedFields = DefaultEntityFieldsUtil.entityFieldsFromJson(selectedFieldsJson.toString());
            if (selectedFields.get(Entity.getEntityType(entityModel)) == null) {
                selectedFields = defaultFields;
            }
        }

        JPanel rootPanel = new JPanel();
        rootPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
        add(rootPanel, BorderLayout.CENTER);
        GridBagLayout gbl_rootPanel = new GridBagLayout();
        gbl_rootPanel.columnWidths = new int[] { 0, 0 };
        gbl_rootPanel.rowHeights = new int[] { 0, 0, 0, 0 };
        gbl_rootPanel.columnWeights = new double[] { 1.0, Double.MIN_VALUE };
        gbl_rootPanel.rowWeights = new double[] { 0.0, 0.0, 0.0, 1.0 };
        rootPanel.setLayout(gbl_rootPanel);

        headerPanel = new HeaderPanel();
        headerPanel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_headerPanel = new GridBagConstraints();
        gbc_headerPanel.insets = new Insets(0, 0, 5, 0);
        gbc_headerPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_headerPanel.gridx = 0;
        gbc_headerPanel.gridy = 0;
        rootPanel.add(headerPanel, gbc_headerPanel);

        entityFieldsPanel = new EntityFieldsPanel(entityModel, fields);
        drawSpecificDetailsForEntity(entityModel);

        JXPanel entityDetailsAndCommentsPanel = new JXPanel();
        GridBagConstraints gbc_entityDetailsAndCommentsPanel = new GridBagConstraints();
        gbc_entityDetailsAndCommentsPanel.insets = new Insets(10, 0, 5, 0);
        gbc_entityDetailsAndCommentsPanel.fill = GridBagConstraints.BOTH;
        gbc_entityDetailsAndCommentsPanel.gridx = 0;
        gbc_entityDetailsAndCommentsPanel.gridy = 1;
        rootPanel.add(entityDetailsAndCommentsPanel, gbc_entityDetailsAndCommentsPanel);

        GridBagLayout gbl_entityDetailsAndCommentsPanel = new GridBagLayout();
        gbl_entityDetailsAndCommentsPanel.columnWidths = new int[] { 0, 0, 0 };
        gbl_entityDetailsAndCommentsPanel.rowHeights = new int[] { 0, 0 };
        gbl_entityDetailsAndCommentsPanel.columnWeights = new double[] { 1.0, 0.0, Double.MIN_VALUE };
        gbl_entityDetailsAndCommentsPanel.rowWeights = new double[] { 0.0, Double.MIN_VALUE };
        entityDetailsAndCommentsPanel.setLayout(gbl_entityDetailsAndCommentsPanel);

        GridBagConstraints gbc_entityDetailsPanel = new GridBagConstraints();
        gbc_entityDetailsPanel.fill = GridBagConstraints.BOTH;
        gbc_entityDetailsPanel.gridx = 0;
        gbc_entityDetailsPanel.gridy = 0;
        entityDetailsAndCommentsPanel.add(entityFieldsPanel, gbc_entityDetailsPanel);

        commentsDetails = new JXPanel();
        commentsDetails.setVisible(false);
        commentsDetails.setLayout(new BorderLayout());
        commentsDetails.setMinimumSize(new Dimension(400, 200));
        commentsDetails.setScrollableTracksViewportWidth(false);

        commentsListPanel = new CommentsConversationPanel(baseUrl);
        commentsListPanel.setPreferredSize(new Dimension(200, 68));
        commentsListPanel.setMaximumSize(new Dimension(200, 200));
        commentsListPanel.setBorder(new MatteBorder(1, 1, 1, 1, JBColor.border()));
        commentsDetails.add(commentsListPanel, BorderLayout.CENTER);

        GridBagConstraints gbc_commentsPanel = new GridBagConstraints();
        gbc_commentsPanel.fill = GridBagConstraints.VERTICAL;
        gbc_commentsPanel.gridx = 1;
        gbc_commentsPanel.gridy = 0;
        entityDetailsAndCommentsPanel.add(commentsDetails, gbc_commentsPanel);

        label = new JXLabel();
        label.setText("Description");
        label.setFont(new Font("Arial", Font.BOLD, 15));
        GridBagConstraints gbc_label = new GridBagConstraints();
        gbc_label.fill = GridBagConstraints.HORIZONTAL;
        gbc_label.insets = new Insets(0, 0, 5, 0);
        gbc_label.gridx = 0;
        gbc_label.gridy = 2;
        rootPanel.add(label, gbc_label);

        descriptionDetails = new HTMLPresenterFXPanel(baseUrl);
        descriptionDetails.setPreferredSize(new Dimension(0, 120));
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.NORTH;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridx = 0;
        gbc.gridy = 3;
        rootPanel.add(descriptionDetails, gbc);

        drawGeneralDetailsForEntity(entityModel);
        descriptionDetails.addEventActions();
    }

    private void drawGeneralDetailsForEntity(EntityModel entityModel) {
        final String descriptionContent = Util.getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DESCRIPTION));

        // Setting header phase
        headerPanel.setPhaseDetails(Util.getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PHASE)));

        // Setting description content
        Platform.runLater(() -> descriptionDetails.setContent(descriptionContent));
        Platform.runLater(() -> descriptionDetails.initFX());
    }

    public void setRefreshButton(AnAction refreshButton) {
        headerPanel.setRefreshButton(refreshButton);
    }

    public void setCommentsButton(AnAction commentsButton) {
        headerPanel.setCommentButton(commentsButton);
    }

    public void setSaveSelectedPhaseButton(AnAction saveSelectedPhaseAction) {
        headerPanel.setSaveButton(saveSelectedPhaseAction);
    }

    public void removeSaveSelectedPhaseButton() {
        headerPanel.removeSaveSelectedPhaseButton();
    }

    public void openInBrowserButton(AnAction openInBrowserAction) {
        headerPanel.setOpenInBrowserButton(openInBrowserAction);
    }

    public void setPhaseInHeader(boolean showPhase) {
        headerPanel.setPhaseInHeader(showPhase);
    }

    public EntityModel getSelectedTransition() {
        return headerPanel.getSelectedTransition();
    }

    private void drawSpecificDetailsForEntity(EntityModel entityModel) {
        EntityIconFactory entityIconFactory = new EntityIconFactory(26, 26, 12);
        headerPanel.setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(Entity.getEntityType(entityModel))));
        headerPanel.setId(Util.getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ID)));
        headerPanel.setNameDetails(Util.getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_NAME)));
        entityFieldsPanel.createSectionWithEntityDetails(entityModel, selectedFields.get(Entity.getEntityType(entityModel)));
    }

    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
        headerPanel.setPossiblePhasesForEntity(phasesList);
    }

    public void setComments(Collection<EntityModel> comments) {
        commentsListPanel.clearCurrentComments();
        for (EntityModel comment : comments) {
            String commentsPostTime = Util.getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_CREATION_TIME));
            String userName = Util.getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_AUTHOR), "full_name");
            String commentLine = Util.getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_COMMENT_TEXT));
            commentsListPanel.addExistingComment(commentsPostTime, userName, commentLine);
        }
        commentsListPanel.setChatBoxScene();
    }

    public void addSendNewCommentAction(ActionListener actionListener) {
        commentsListPanel.addSendNewCommentAction(actionListener);
    }

    public void activateFieldsSettings() {
        fieldsPopup.setLocation(headerPanel.getFieldsPopupLocation().x - (int) fieldsPopup.getPreferredSize().getWidth(),
                headerPanel.getFieldsPopupLocation().y);
        fieldsPopup.setVisible(!fieldsPopup.isVisible());
    }

    public void setCommentMessageBoxText(String t) {
        commentsListPanel.setCommentMessageBoxText(t);
    }

    public String getCommentMessageBoxText() {
        return commentsListPanel.getCommentMessageBoxText();
    }

    public void activateCommentsCollapsible() {
        commentsDetails.setVisible(!commentsDetails.isVisible());
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
        fieldsPopup.addSelectionListener(e -> entityFieldsPanel.createSectionWithEntityDetails(entityModel, fieldsPopup.getSelectedFields()));
        fieldsPopup.addSelectionListener(selectionListener);
    }

    public void addFieldSelectListener(FieldsSelectFrame.SelectionListener selectionListener) {
        this.selectionListener = selectionListener;
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
