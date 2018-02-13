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
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork.NoWorkPanel;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.DefaultEntityFieldsUtil;
import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.DataKeys;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.JBColor;
import javafx.application.Platform;
import org.jdesktop.swingx.JXCollapsiblePane;
import org.jdesktop.swingx.JXCollapsiblePane.Direction;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextField;
import org.json.JSONObject;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;

import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.*;
import java.awt.*;
import java.util.List;
import java.util.stream.Collectors;

import static com.hpe.adm.octane.ideplugins.services.util.Util.getUiDataFromModel;

public class GeneralEntityDetailsPanel extends JPanel implements Scrollable {

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private Map<Entity, Set<String>> defaultFields = DefaultEntityFieldsUtil.getDefaultFields();
    private Map<Entity, Set<String>> selectedFields;
    private JXPanel entityDetailsPanel;
    private JXPanel commentsDetails;
    private FieldsSelectFrame fieldsPopup;
    private JXPanel detailsPanelLeft;
    private JXPanel detailsPanelRight;
    private HTMLPresenterFXPanel descriptionDetails;
    private EntityModel entityModel;
    private Collection<FieldMetadata> fields;
    private HeaderPanel headerPanel;
    private CommentsConversationPanel commentsListPanel;
    private JXLabel label;
    private List<GenericField> genericFieldList;

    private FieldsSelectFrame.SelectionListener selectionListener;


    public GeneralEntityDetailsPanel(EntityModel entityModel, Collection<FieldMetadata> fields) {
        setLayout(new BorderLayout(0, 0));

        this.entityModel = entityModel;
        this.fields = fields;
        genericFieldList = new ArrayList<>();

        //create the generic fields
        fields.forEach(e -> genericFieldList.add(new GenericField(e)));

        DataManager dataManager = DataManager.getInstance();
        @SuppressWarnings("deprecation")
        DataContext dataContext = dataManager.getDataContext();
        Project project = DataKeys.PROJECT.getData(dataContext);

        idePluginPersistentState = PluginModule.getInstance(project, IdePluginPersistentState.class);

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
        gbl_rootPanel.columnWidths = new int[]{0, 0};
        gbl_rootPanel.rowHeights = new int[]{0, 0, 0, 0};
        gbl_rootPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_rootPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 1.0};
        rootPanel.setLayout(gbl_rootPanel);

        headerPanel = new HeaderPanel();
        headerPanel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_headerPanel = new GridBagConstraints();
        gbc_headerPanel.insets = new Insets(0, 0, 5, 0);
        gbc_headerPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_headerPanel.gridx = 0;
        gbc_headerPanel.gridy = 0;
        rootPanel.add(headerPanel, gbc_headerPanel);


        entityDetailsPanel = createMainPanel();

        detailsPanelLeft = createLeftPanel(entityDetailsPanel);
        detailsPanelRight = createRightPanel(entityDetailsPanel);
        addComponentListener(entityDetailsPanel, detailsPanelLeft, detailsPanelRight);
        drawSpecificDetailsForEntity(entityModel);
        entityDetailsPanel.setBorder(new EmptyBorder(0, 0, 0, 10));

        JPanel paddingPanel = new JPanel();
        paddingPanel.setMinimumSize(new Dimension(50, 50));
        GridBagConstraints gbc_paddingPanel = new GridBagConstraints();
        gbc_paddingPanel.anchor = GridBagConstraints.SOUTHWEST;
        gbc_paddingPanel.insets = new Insets(0, 0, 0, 0);
        gbc_paddingPanel.gridx = 0;
        gbc_paddingPanel.gridy = 2;
        gbc_paddingPanel.gridwidth = 2;
        entityDetailsPanel.add(paddingPanel, gbc_paddingPanel);


        JXLabel sectionTitle = new JXLabel();
        sectionTitle.setFont(new Font("Arial", Font.BOLD, 18));
        sectionTitle.setText("General");
        GridBagConstraints gbc_title = new GridBagConstraints();
        gbc_title.anchor = GridBagConstraints.SOUTHWEST;
        gbc_title.insets = new Insets(0, 0, 0, 0);
        gbc_title.gridx = 0;
        gbc_title.gridy = 0;
        entityDetailsPanel.add(sectionTitle, gbc_title);

        JXPanel entityDetailsAndCommentsPanel = new JXPanel();
        GridBagConstraints gbc_entityDetailsAndCommentsPanel = new GridBagConstraints();
        gbc_entityDetailsAndCommentsPanel.insets = new Insets(10, 10, 5, 0);
        gbc_entityDetailsAndCommentsPanel.fill = GridBagConstraints.BOTH;
        gbc_entityDetailsAndCommentsPanel.gridx = 0;
        gbc_entityDetailsAndCommentsPanel.gridy = 1;
        rootPanel.add(entityDetailsAndCommentsPanel, gbc_entityDetailsAndCommentsPanel);
        GridBagLayout gbl_entityDetailsAndCommentsPanel = new GridBagLayout();
        gbl_entityDetailsAndCommentsPanel.columnWidths = new int[]{0, 0, 0};
        gbl_entityDetailsAndCommentsPanel.rowHeights = new int[]{0, 0};
        gbl_entityDetailsAndCommentsPanel.columnWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
        gbl_entityDetailsAndCommentsPanel.rowWeights = new double[]{0.0, Double.MIN_VALUE};
        entityDetailsAndCommentsPanel.setLayout(gbl_entityDetailsAndCommentsPanel);
        GridBagConstraints gbc_entityDetailsPanel = new GridBagConstraints();
        gbc_entityDetailsPanel.fill = GridBagConstraints.BOTH;
        gbc_entityDetailsPanel.gridx = 0;
        gbc_entityDetailsPanel.gridy = 0;
        entityDetailsAndCommentsPanel.add(entityDetailsPanel, gbc_entityDetailsPanel);

        GridBagConstraints gbc_descriptionDetails_1 = new GridBagConstraints();
        gbc_descriptionDetails_1.insets = new Insets(0, 10, 5, 0);
        gbc_descriptionDetails_1.fill = GridBagConstraints.BOTH;
        gbc_descriptionDetails_1.gridx = 0;
        gbc_descriptionDetails_1.gridy = 1;

        commentsDetails = new JXPanel();
        commentsDetails.setVisible(false);
        commentsDetails.setLayout(new BorderLayout());
        commentsDetails.setMinimumSize(new Dimension(400,200));
        commentsDetails.setScrollableTracksViewportWidth(false);

        commentsListPanel = new CommentsConversationPanel();
        commentsListPanel.setPreferredSize(new Dimension(400, (int) entityDetailsPanel.getPreferredSize().getHeight() + 50));
        commentsListPanel.setMaximumSize(new Dimension(400, 200));
        commentsListPanel.setBorder(new MatteBorder(1, 1, 1, 1, JBColor.border()));
        commentsDetails.add(commentsListPanel);


        GridBagConstraints gbc_commentsPanel = new GridBagConstraints();
        gbc_commentsPanel.fill = GridBagConstraints.BOTH;
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

        descriptionDetails = new HTMLPresenterFXPanel();
        descriptionDetails.setPreferredSize(new Dimension(0, 120));
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridx = 0;
        gbc.gridy = 3;
        rootPanel.add(descriptionDetails, gbc);

        drawGeneralDetailsForEntity(entityModel);
        descriptionDetails.addEventActions();
    }

    private void drawGeneralDetailsForEntity(EntityModel entityModel) {
        final String descriptionContent = getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DESCRIPTION));

        //Setting header phase
        headerPanel.setPhaseDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PHASE)));

        //Setting description content
        Platform.runLater(() -> descriptionDetails.setContent(descriptionContent));
        Platform.runLater(() -> descriptionDetails.initFX());
    }


    public void setEntityNameClickHandler(Runnable runnable) {
        headerPanel.setActionToEntityLink(runnable);
    }

    public void setRefreshButton(AnAction refreshButton) {
        headerPanel.setRefreshButton(refreshButton);
    }

    public void setCommentsButton(AnAction commentsButton) {
        headerPanel.setCommentButton(commentsButton);
    }

    public void setSaveSelectedPhaseButton(AnAction saveSelectedPhaseAction) {
        headerPanel.setSaveSelectedPhaseButton(saveSelectedPhaseAction);
    }

    public void removeSaveSelectedPhaseButton() {
        headerPanel.removeSaveSelectedPhaseButton();
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
        headerPanel.setNameDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_NAME)));

        GenericField gField = genericFieldList.stream().filter(f -> f.getFieldName().equals("name")).limit(1).collect(Collectors.toList()).get(0);
        headerPanel.setNameFieldListener(e -> gField.updateField(entityModel, headerPanel.getNameFieldValue()));
        createSectionWithEntityDetails(entityModel, selectedFields.get(Entity.getEntityType(entityModel)));
    }


    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
        headerPanel.setPossiblePhasesForEntity(phasesList);
    }

    public void setComments(Collection<EntityModel> comments) {
        commentsListPanel.clearCurrentComments();
        for (EntityModel comment : comments) {
            String commentsPostTime = getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_CREATION_TIME));
            String userName = getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_AUTHOR), "full_name");
            String commentLine = getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_COMMENT_TEXT));
            commentsListPanel.addExistingComment(commentsPostTime, userName, commentLine);
        }
        commentsListPanel.setChatBoxScene();
    }

    public void addSendNewCommentAction(ActionListener actionListener) {
        commentsListPanel.addSendNewCommentAction(actionListener);
    }

    public void activateFieldsSettings() {
        fieldsPopup.setLocation(headerPanel.getFieldsPopupLocation().x - (int) fieldsPopup.getPreferredSize().getWidth(), headerPanel.getFieldsPopupLocation().y);
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

    public void setFieldSelectButton(EntityDetailPresenter.SelectFieldsAction fieldSelectButton) {
        headerPanel.setFieldSelectButton(fieldSelectButton);
        fieldsPopup = new FieldsSelectFrame(defaultFields.get(Entity.getEntityType(entityModel)),
                fields,
                selectedFields,
                Entity.getEntityType(entityModel),
                idePluginPersistentState,
                fieldSelectButton);
        fieldsPopup.addSelectionListener(e -> createSectionWithEntityDetails(entityModel, fieldsPopup.getSelectedFields()));
        fieldsPopup.addSelectionListener(selectionListener);
    }

    public void addFieldSelectListener(FieldsSelectFrame.SelectionListener selectionListener) {
        this.selectionListener = selectionListener;
    }

    public void createSectionWithEntityDetails(EntityModel entityModel, Set<String> fieldNames) {
        detailsPanelLeft.removeAll();
        detailsPanelRight.removeAll();

        int fieldCount = 0;
        int i = 0;
        for (GenericField gField : genericFieldList) {
            if (fieldNames.contains(gField.getFieldName())) {
                JXLabel fieldLabel = new JXLabel();
                fieldLabel.setFont(new Font("Arial", Font.BOLD, 12));
                fieldLabel.setText(gField.getFieldLabel());
                GridBagConstraints gbc1 = new GridBagConstraints();
                gbc1.anchor = GridBagConstraints.SOUTHWEST;
                gbc1.insets = new Insets(10, 0, 0, 0);
                gbc1.fill = GridBagConstraints.HORIZONTAL;
                gbc1.gridx = 0;
                gbc1.gridy = i;

                String fieldValue = getUiDataFromModel(entityModel.getValue(gField.getFieldName()));

                JXTextField fieldValueLabel = new JXTextField();
                fieldValueLabel.setFont(new Font("Arial", Font.PLAIN, 12));
                fieldValueLabel.setText(fieldValue);
                fieldValueLabel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
                fieldValueLabel.setToolTipText(fieldValue);
                GridBagConstraints gbc2 = new GridBagConstraints();

                if (gField.isEditable()) {
                    fieldValueLabel.addActionListener(e -> gField.updateField(entityModel, fieldValueLabel.getText()));
                } else {
                    fieldValueLabel.setEnabled(false);
                }
                fieldValueLabel.setBackground(detailsPanelLeft.getBackground());

                gbc2.insets = new Insets(10, 10, 0, 5);
                gbc2.anchor = GridBagConstraints.SOUTHWEST;
                gbc2.fill = GridBagConstraints.HORIZONTAL;
                gbc2.gridx = 1;
                gbc2.gridy = i;

                //edit label grid bag layout
                GridBagConstraints gbc3 = new GridBagConstraints();
                gbc3.anchor = GridBagConstraints.SOUTHWEST;
                gbc3.insets = new Insets(10, 0, 0, 0);
                gbc3.fill = GridBagConstraints.HORIZONTAL;
                gbc3.gridx = 2;
                gbc3.gridy = i;


                if (fieldCount % 2 == 0) {
                    detailsPanelLeft.add(fieldLabel, gbc1);
                    detailsPanelLeft.add(fieldValueLabel, gbc2);
                    if (gField.isEditable())
                        detailsPanelLeft.add(new JXLabel(IconLoader.findIcon(Constants.IMG_EDIT_LOGO)), gbc3);
                } else {
                    detailsPanelRight.add(fieldLabel, gbc1);
                    detailsPanelRight.add(fieldValueLabel, gbc2);
                    if (gField.isEditable())
                        detailsPanelRight.add(new JXLabel(IconLoader.findIcon(Constants.IMG_EDIT_LOGO)), gbc3);
                }
                i++;
                fieldCount++;
            }
        }
        detailsPanelLeft.repaint();
        detailsPanelLeft.revalidate();
        detailsPanelRight.repaint();
        detailsPanelRight.revalidate();
    }


    private JXPanel createLeftPanel(JXPanel mainPanel) {
        JXPanel detailsPanelLeft = new JXPanel();
        detailsPanelLeft.setBorder(null);
        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.anchor = GridBagConstraints.NORTH;
        gbc1.fill = GridBagConstraints.HORIZONTAL;
        gbc1.insets = new Insets(10, 0, 15, 0);
        gbc1.gridx = 0;
        gbc1.gridy = 1;
        mainPanel.add(detailsPanelLeft, gbc1);
        GridBagLayout gbl_detailsPanelLeft = new GridBagLayout();
        gbl_detailsPanelLeft.columnWidths = new int[]{0, 0, 0};
        gbl_detailsPanelLeft.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelLeft.columnWeights = new double[]{0.0, 1.0, 0.0};
        gbl_detailsPanelLeft.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelLeft.setLayout(gbl_detailsPanelLeft);
        return detailsPanelLeft;
    }

    private JXPanel createRightPanel(JXPanel mainPanel) {
        JXPanel detailsPanelRight = new JXPanel();
        detailsPanelRight.setBorder(null);
        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.anchor = GridBagConstraints.NORTH;
        gbc1.insets = new Insets(10, 10, 15, 0);
        gbc1.gridx = 1;
        gbc1.gridy = 1;
        gbc1.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(detailsPanelRight, gbc1);
        GridBagLayout gbl_detailsPanelRight = new GridBagLayout();
        gbl_detailsPanelRight.columnWidths = new int[]{0, 0, 0};
        gbl_detailsPanelRight.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelRight.columnWeights = new double[]{0.0, 1.0, 0.0};
        gbl_detailsPanelRight.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelRight.setLayout(gbl_detailsPanelRight);
        return detailsPanelRight;
    }

    private void addComponentListener(JXPanel mainPanel, JXPanel leftPanel, JXPanel rightPanel) {
        mainPanel.addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent e) {
                int halfWidth = mainPanel.getWidth() / 2;
                int height = mainPanel.getHeight();
                if (halfWidth != 0 && height != 0) {
                    leftPanel.setPreferredSize(new Dimension((int) halfWidth, height));
                    rightPanel.setPreferredSize(new Dimension((int) halfWidth, height));
                    mainPanel.updateUI();
                    mainPanel.repaint();
                }
            }
        });
    }

    private JXPanel createMainPanel() {
        JXPanel detailsPanelMain = new JXPanel();
        detailsPanelMain.setBorder(null);
        GridBagLayout mainPaneGrid = new GridBagLayout();
        mainPaneGrid.columnWidths = new int[]{0, 0};
        mainPaneGrid.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        mainPaneGrid.columnWeights = new double[]{0.5, 0.5};
        mainPaneGrid.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelMain.setLayout(mainPaneGrid);
        return detailsPanelMain;
    }

    public Set<String> getSelectedFields() {
        return fieldsPopup.getSelectedFields();
    }

    public void setSelectedFields(Set<String> selectedFields) {
        fieldsPopup.setSelectedFieldsFromOtherTab(selectedFields);
    }

    public EntityModel getEntityModel() {
        return entityModel;
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
