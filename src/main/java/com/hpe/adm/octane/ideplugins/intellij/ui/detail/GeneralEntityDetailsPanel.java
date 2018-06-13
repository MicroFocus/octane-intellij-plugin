package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import java.awt.Color;
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
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Scrollable;
import javax.swing.border.MatteBorder;

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

import javafx.application.Platform;

public class GeneralEntityDetailsPanel extends JPanel implements Scrollable {

    private HeaderPanel headerPanel;
    private EntityFieldsPanel entityFieldsPanel;
    private CommentsConversationPanel commentsPanel;
    private HTMLPresenterFXPanel descriptionPanel;
    private FieldsSelectFrame fieldsPopup;
    private FieldsSelectFrame.SelectionListener selectionListener;

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private Map<Entity, Set<String>> defaultFields = DefaultEntityFieldsUtil.getDefaultFields();
    private Map<Entity, Set<String>> selectedFields;
    private Collection<FieldMetadata> fields;
    private String baseUrl;
    private EntityModel entityModel;
    private JLabel descriptionLabel;

    public GeneralEntityDetailsPanel(EntityModel entityModel, Collection<FieldMetadata> fields) {
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

        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[] { 0, 0, 0 };
        gridBagLayout.rowHeights = new int[] { 0, 0, 0, 0, 0 };
        gridBagLayout.columnWeights = new double[] { 1.0, 0.0, Double.MIN_VALUE };
        gridBagLayout.rowWeights = new double[] { 0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE };
        setLayout(gridBagLayout);

        headerPanel = new HeaderPanel();
        headerPanel.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_headerPanel = new GridBagConstraints();
        gbc_headerPanel.gridwidth = 2;
        gbc_headerPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_headerPanel.insets = new Insets(5, 0, 5, 5);
        gbc_headerPanel.gridx = 0;
        gbc_headerPanel.gridy = 0;
        add(headerPanel, gbc_headerPanel);

        entityFieldsPanel = new EntityFieldsPanel(fields);
        GridBagConstraints gbc_entityFieldsPanel = new GridBagConstraints();
        gbc_entityFieldsPanel.weightx = 6.0;
        gbc_entityFieldsPanel.fill = GridBagConstraints.BOTH;
        gbc_entityFieldsPanel.insets = new Insets(0, 5, 0, 5);
        gbc_entityFieldsPanel.gridx = 0;
        gbc_entityFieldsPanel.gridy = 1;
        add(entityFieldsPanel, gbc_entityFieldsPanel);

        commentsPanel = new CommentsConversationPanel(baseUrl);
        GridBagConstraints gbc_commentsPanel = new GridBagConstraints();
        gbc_commentsPanel.weightx = 2.0;
        gbc_commentsPanel.gridheight = 3;
        gbc_commentsPanel.fill = GridBagConstraints.BOTH;
        gbc_commentsPanel.insets = new Insets(0, 5, 0, 5);
        gbc_commentsPanel.gridx = 1;
        gbc_commentsPanel.gridy = 1;
        add(commentsPanel, gbc_commentsPanel);
        commentsPanel.setVisible(false);

        descriptionLabel = new JLabel("Description");
        descriptionLabel.setFont(new Font("Arial", Font.BOLD, 18));
        GridBagConstraints gbc_descriptionLabel = new GridBagConstraints();
        gbc_descriptionLabel.fill = GridBagConstraints.HORIZONTAL;
        gbc_descriptionLabel.weightx = 6.0;
        gbc_descriptionLabel.insets = new Insets(5, 5, 5, 5);
        gbc_descriptionLabel.gridx = 0;
        gbc_descriptionLabel.gridy = 2;
        add(descriptionLabel, gbc_descriptionLabel);

        descriptionPanel = new HTMLPresenterFXPanel(baseUrl);
        GridBagConstraints gbc_descriptionPanel = new GridBagConstraints();
        gbc_descriptionPanel.weightx = 6.0;
        gbc_descriptionPanel.fill = GridBagConstraints.BOTH;
        gbc_descriptionPanel.insets = new Insets(0, 0, 0, 5);
        gbc_descriptionPanel.gridx = 0;
        gbc_descriptionPanel.gridy = 3;
        add(descriptionPanel, gbc_descriptionPanel);

        drawGeneralDetailsForEntity(entityModel);
        descriptionPanel.addEventActions();

    }

    private void drawGeneralDetailsForEntity(EntityModel entityModel) {
        final String descriptionContent = Util.getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DESCRIPTION));

        // Setting header phase
        headerPanel.setPhaseDetails(Util.getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PHASE)));

        // Setting description content
        Platform.runLater(() -> descriptionPanel.setContent(descriptionContent));
        Platform.runLater(() -> descriptionPanel.initFX());
        drawSpecificDetailsForEntity(entityModel);

    }

    private void drawSpecificDetailsForEntity(EntityModel entityModel) {
        EntityIconFactory entityIconFactory = new EntityIconFactory(26, 26, 12);
//        headerPanel.setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(Entity.getEntityType(entityModel))));
        headerPanel.setId(Util.getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ID)));
        headerPanel.setNameDetails(Util.getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_NAME)));
        entityFieldsPanel.createSectionWithEntityDetails(entityModel, selectedFields.get(Entity.getEntityType(entityModel)));
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

    public void activateFieldsSettings() {
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

    public void activateCommentsCollapsible() {
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
