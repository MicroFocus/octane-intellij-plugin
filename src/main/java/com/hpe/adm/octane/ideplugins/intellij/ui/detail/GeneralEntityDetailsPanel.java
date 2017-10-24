/*
 * Copyright 2017 Hewlett-Packard Enterprise Development Company, L.P.
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

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.MetadataService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.ui.FormField;
import com.hpe.adm.octane.ideplugins.services.ui.FormLayout;
import com.hpe.adm.octane.ideplugins.services.ui.FormLayoutSection;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.ui.JBColor;
import javafx.application.Platform;
import org.jdesktop.swingx.JXCollapsiblePane;
import org.jdesktop.swingx.JXCollapsiblePane.Direction;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.UnsupportedEncodingException;
import java.util.Collection;
import java.util.List;

import static com.hpe.adm.octane.ideplugins.services.util.Util.getUiDataFromModel;

public class GeneralEntityDetailsPanel extends JPanel implements Scrollable {

    private JXPanel entityDetailsPanel;
    private JXCollapsiblePane commentsDetails;
    private HTMLPresenterFXPanel descriptionDetails;
    private boolean hasAttachment;
    private MetadataService metadataService;
    private HeaderPanel headerPanel;
    private CommentsConversationPanel commentsListPanel;
    private JXLabel label;

    private FormLayout octaneEntityForm;

    public GeneralEntityDetailsPanel(MetadataService metadataService, EntityModel entityModel) {
        setLayout(new BorderLayout(0, 0));

        this.metadataService = metadataService;
        try {
            octaneEntityForm = metadataService.getFormLayoutForSpecificEntityType(Entity.getEntityType(entityModel));
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
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

        entityDetailsPanel = drawSpecificDetailsForEntity(entityModel);
        entityDetailsPanel.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_entityDetailsPanel = new GridBagConstraints();
        gbc_entityDetailsPanel.fill = GridBagConstraints.BOTH;
        gbc_entityDetailsPanel.gridx = 0;
        gbc_entityDetailsPanel.gridy = 0;

        JXPanel entityDetailsAndCommentsPanel = new JXPanel();
        entityDetailsAndCommentsPanel.setPreferredSize(new Dimension((int) entityDetailsPanel.getPreferredSize().getWidth(), (int) entityDetailsPanel.getPreferredSize().getHeight() + 50));
        GridBagConstraints gbc_entityDetailsAndCommentsPanel = new GridBagConstraints();
        gbc_entityDetailsAndCommentsPanel.insets = new Insets(0, 0, 5, 0);
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
        entityDetailsAndCommentsPanel.add(entityDetailsPanel, gbc_entityDetailsPanel);


        GridBagConstraints gbc_descriptionDetails_1 = new GridBagConstraints();
        gbc_descriptionDetails_1.insets = new Insets(0, 0, 5, 0);
        gbc_descriptionDetails_1.fill = GridBagConstraints.BOTH;
        gbc_descriptionDetails_1.gridx = 0;
        gbc_descriptionDetails_1.gridy = 1;

        commentsDetails = new JXCollapsiblePane(Direction.LEFT);
        commentsDetails.setCollapsed(true);
        commentsDetails.setLayout(new BorderLayout());

        commentsListPanel = new CommentsConversationPanel();
        commentsListPanel.setPreferredSize(new Dimension(400, (int) entityDetailsPanel.getPreferredSize().getHeight() + 50));
        commentsListPanel.setMaximumSize(new Dimension(400, 200));
        commentsListPanel.setBorder(new MatteBorder(1, 1, 1, 1, JBColor.border()));
        commentsDetails.getContentPane().add(commentsListPanel);

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

    private JXPanel drawSpecificDetailsForEntity(EntityModel entityModel) {
        EntityIconFactory entityIconFactory = new EntityIconFactory(26, 26, 12);
        headerPanel.setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(Entity.getEntityType(entityModel))));
        headerPanel.setNameDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_NAME)));
        hasAttachment = false;
        JXPanel ret = createMainPanel();
        int sections = 0;
        for (FormLayoutSection formLayoutSection : octaneEntityForm.getFormLayoutSections()) {
            createSectionWithEntityDetails(sections, ret, entityModel, formLayoutSection);
            sections += 2;
        }
        return ret;
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

    public void setCommentMessageBoxText(String t) {
        commentsListPanel.setCommentMessageBoxText(t);
    }

    public String getCommentMessageBoxText() {
        return commentsListPanel.getCommentMessageBoxText();
    }

    public void activateCollapsible() {
        commentsDetails.setCollapsed(!commentsDetails.isCollapsed());
    }

    public void createSectionWithEntityDetails(int row, JXPanel mainPanel, EntityModel entityModel, FormLayoutSection formSection) {

        //get the list of fields from the formsection
        List<FormField> formFields = formSection.getFields();

        //create section title label
        JXLabel sectionTitle = new JXLabel();
        sectionTitle.setFont(new Font("Arial", Font.BOLD, 18));
        sectionTitle.setText(formSection.getSectionTitle());
        GridBagConstraints gbc_title = new GridBagConstraints();
        gbc_title.anchor = GridBagConstraints.SOUTHWEST;
        gbc_title.insets = new Insets(0, 0, 0, 0);
        gbc_title.gridx = 0;
        gbc_title.gridy = row++;
        mainPanel.add(sectionTitle, gbc_title);

        //create the right and left panels
        JXPanel detailsPanelLeft = createLeftPanel(row, mainPanel);
        JXPanel detailsPanelRight = createRightPanel(row, mainPanel);
        addComponentListener(mainPanel, detailsPanelLeft, detailsPanelRight);

        int fieldCount = 0;
        for (int i = 0; i < formFields.size(); i++) {
            String fieldName = formFields.get(i).getName();

            if (!"description".equals(fieldName)) {

                JXLabel field = new JXLabel();
                field.setFont(new Font("Arial", Font.BOLD, 12));
                field.setText(prettifyLabels(fieldName));
                GridBagConstraints gbc1 = new GridBagConstraints();
                gbc1.anchor = GridBagConstraints.SOUTHWEST;
                gbc1.insets = new Insets(10, 0, 0, 0);
                gbc1.fill = GridBagConstraints.HORIZONTAL;
                gbc1.gridx = 0;
                gbc1.gridy = i;

                String fieldValue = getUiDataFromModel(entityModel.getValue(fieldName));
                JXLabel fieldValueLabel = new JXLabel();
                fieldValueLabel.setFont(new Font("Arial", Font.PLAIN, 12));
                fieldValueLabel.setText(fieldValue);
                fieldValueLabel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
                fieldValueLabel.setToolTipText(fieldValue);
                GridBagConstraints gbc2 = new GridBagConstraints();
                gbc2.insets = new Insets(10, 10, 0, 5);
                gbc2.anchor = GridBagConstraints.SOUTHWEST;
                gbc2.fill = GridBagConstraints.HORIZONTAL;
                gbc2.gridx = 1;
                gbc2.gridy = i;

                if (fieldCount % 2 == 0) {
                    detailsPanelLeft.add(field, gbc1);
                    detailsPanelLeft.add(fieldValueLabel, gbc2);
                } else {
                    detailsPanelRight.add(field, gbc1);
                    detailsPanelRight.add(fieldValueLabel, gbc2);
                }

                fieldCount++;
            }
        }
    }

    private JXPanel createLeftPanel(int row, JXPanel mainPanel) {
        JXPanel detailsPanelLeft = new JXPanel();
        detailsPanelLeft.setBorder(null);
        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.anchor = GridBagConstraints.NORTH;
        gbc1.fill = GridBagConstraints.HORIZONTAL;
        gbc1.insets = new Insets(10, 0, 15, 0);
        gbc1.gridx = 0;
        gbc1.gridy = row;
        mainPanel.add(detailsPanelLeft, gbc1);
        GridBagLayout gbl_detailsPanelLeft = new GridBagLayout();
        gbl_detailsPanelLeft.columnWidths = new int[]{0, 0};
        gbl_detailsPanelLeft.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelLeft.columnWeights = new double[]{0.0, 1.0};
        gbl_detailsPanelLeft.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelLeft.setLayout(gbl_detailsPanelLeft);

        return detailsPanelLeft;
    }

    private JXPanel createRightPanel(int row, JXPanel mainPanel) {
        JXPanel detailsPanelRight = new JXPanel();
        detailsPanelRight.setBorder(null);
        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.anchor = GridBagConstraints.NORTH;
        gbc1.insets = new Insets(10, 10, 15, 0);
        gbc1.gridx = 1;
        gbc1.gridy = row;
        gbc1.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(detailsPanelRight, gbc1);
        GridBagLayout gbl_detailsPanelRight = new GridBagLayout();
        gbl_detailsPanelRight.columnWidths = new int[]{0, 0};
        gbl_detailsPanelRight.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanelRight.columnWeights = new double[]{0.0, 1.0};
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
        JXPanel rootPanel = new JXPanel();
        rootPanel.setBorder(null);
        rootPanel.setLayout(new BorderLayout(0, 0));
        JXPanel detailsPanelMain = new JXPanel();
        detailsPanelMain.setBorder(null);
        rootPanel.add(detailsPanelMain, BorderLayout.CENTER);
        GridBagLayout mainPaneGrid = new GridBagLayout();
        mainPaneGrid.columnWidths = new int[]{0, 0};
        mainPaneGrid.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        mainPaneGrid.columnWeights = new double[]{1.0, 1.0};
        mainPaneGrid.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanelMain.setLayout(mainPaneGrid);
        detailsPanelMain.setMinimumSize(new Dimension(0, 0));
        return detailsPanelMain;
    }

    private String prettifyLabels(String str1) {
        //for udfs
        if (str1.contains("_udf")) {
            str1 = metadataService.getUdfLabel(str1);
        }
        str1 = str1.replaceAll("_udf", "");
        str1 = str1.replaceAll("_", " ");
        char[] chars = str1.toCharArray();
        chars[0] = Character.toUpperCase(chars[0]);
        for (int x = 1; x < chars.length; x++) {
            if (chars[x - 1] == ' ') {
                chars[x] = Character.toUpperCase(chars[x]);
            }
        }
        return new String(chars);
    }


    @Override
    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    @Override
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {

        int currentPosition = visibleRect.height;

        return  (getHeight() - currentPosition) / 10;
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
