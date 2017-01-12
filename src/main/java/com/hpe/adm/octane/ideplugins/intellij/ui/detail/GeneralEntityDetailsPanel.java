package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.UrlParser;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.ui.JBColor;
import org.apache.commons.lang.CharEncoding;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextArea;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Entities;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.URI;
import java.util.Collection;

import static com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil.getUiDataFromModel;
import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.*;

public class GeneralEntityDetailsPanel extends JPanel {
    private JXPanel entityDetailsPanel;
    private JXTextArea descriptionDetails;
    private boolean hasAttachment = false;
    private HeaderPanel headerPanel;

    public GeneralEntityDetailsPanel(EntityModel entityModel) {
        setBorder(null);
        setBounds(100, 100, 900, 350);
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0};
        gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{1.0, Double.MIN_VALUE};
        this.setLayout(gridBagLayout);

        JPanel rootPanel = new JPanel();
        rootPanel.setBorder(new EmptyBorder(10, 30, 30, 30));
        rootPanel.setMinimumSize(new Dimension(0, 0));
        GridBagConstraints gbc_rootPanel = new GridBagConstraints();
        gbc_rootPanel.fill = GridBagConstraints.BOTH;
        gbc_rootPanel.gridx = 0;
        gbc_rootPanel.gridy = 0;
        add(rootPanel, gbc_rootPanel);
        GridBagLayout gbl_rootPanel = new GridBagLayout();
        gbl_rootPanel.columnWidths = new int[]{0, 0};
        gbl_rootPanel.rowHeights = new int[]{0, 0, 0, 0, 0};
        gbl_rootPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_rootPanel.rowWeights = new double[]{0.0, 0.0, 1.0, 0.0, Double.MIN_VALUE};
        rootPanel.setLayout(gbl_rootPanel);

        headerPanel = new HeaderPanel();
        headerPanel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        GridBagConstraints gbc_headerPanel = new GridBagConstraints();
        gbc_headerPanel.insets = new Insets(0, 0, 5, 0);
        gbc_headerPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_headerPanel.gridx = 0;
        gbc_headerPanel.gridy = 0;
        rootPanel.add(headerPanel, gbc_headerPanel);

        JXPanel descriptionPanel = new JXPanel();
        descriptionPanel.setBorder(new EmptyBorder(10, 0, 10, 0));
        GridBagConstraints gbc_descriptionPanel = new GridBagConstraints();
        gbc_descriptionPanel.insets = new Insets(0, 0, 5, 0);
        gbc_descriptionPanel.fill = GridBagConstraints.BOTH;
        gbc_descriptionPanel.gridx = 0;
        gbc_descriptionPanel.gridy = 1;
        rootPanel.add(descriptionPanel, gbc_descriptionPanel);
        GridBagLayout gbl_descriptionPanel = new GridBagLayout();
        gbl_descriptionPanel.columnWidths = new int[]{0, 0};
        gbl_descriptionPanel.rowHeights = new int[]{0, 0, 0};
        gbl_descriptionPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_descriptionPanel.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
        descriptionPanel.setLayout(gbl_descriptionPanel);

        JXLabel lblDescription = new JXLabel();
        lblDescription.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDescription.setText("Description");
        GridBagConstraints gbc_lblDescription = new GridBagConstraints();
        gbc_lblDescription.anchor = GridBagConstraints.WEST;
        gbc_lblDescription.insets = new Insets(0, 0, 5, 0);
        gbc_lblDescription.gridx = 0;
        gbc_lblDescription.gridy = 0;
        descriptionPanel.add(lblDescription, gbc_lblDescription);

        descriptionDetails = new JXTextArea();
        descriptionDetails.setLineWrap(true);
        descriptionDetails.setEditable(false);
        descriptionDetails.setOpaque(false);
        descriptionDetails.setLineWrap(true);
        descriptionDetails.setEditable(false);
        descriptionDetails.setBorder(null);
        descriptionDetails.setFont(new Font("Tahoma", Font.PLAIN, 11));
        descriptionDetails.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_descriptionDetails = new GridBagConstraints();
        gbc_descriptionDetails.fill = GridBagConstraints.BOTH;
        gbc_descriptionDetails.gridx = 0;
        gbc_descriptionDetails.gridy = 1;
        descriptionPanel.add(descriptionDetails, gbc_descriptionDetails);

        entityDetailsPanel = drawSpecificDetailsForEntity(entityModel);
        entityDetailsPanel.setBorder(null);
        GridBagConstraints gbc_entityDetailsPanel = new GridBagConstraints();
        gbc_entityDetailsPanel.insets = new Insets(0, 0, 5, 0);
        gbc_entityDetailsPanel.fill = GridBagConstraints.BOTH;
        gbc_entityDetailsPanel.gridx = 0;
        gbc_entityDetailsPanel.gridy = 2;
        rootPanel.add(entityDetailsPanel, gbc_entityDetailsPanel);

        JXPanel atachementsPanel = new JXPanel();
        atachementsPanel.setBorder(new MatteBorder(1, 0, 0, 0, JBColor.border()));
        GridBagConstraints gbc_atachementsPanel = new GridBagConstraints();
        gbc_atachementsPanel.fill = GridBagConstraints.BOTH;
        gbc_atachementsPanel.gridx = 0;
        gbc_atachementsPanel.gridy = 3;
        rootPanel.add(atachementsPanel, gbc_atachementsPanel);
        GridBagLayout gbl_atachementsPanel = new GridBagLayout();
        gbl_atachementsPanel.columnWidths = new int[]{0, 0, 0};
        gbl_atachementsPanel.rowHeights = new int[]{0, 0};
        gbl_atachementsPanel.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        gbl_atachementsPanel.rowWeights = new double[]{0.0, Double.MIN_VALUE};
        atachementsPanel.setLayout(gbl_atachementsPanel);

        JXLabel lblAttachments = new JXLabel();
        lblAttachments.setText("Attachments");
        lblAttachments.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblAttachments.setBorder(new EmptyBorder(5, 0, 0, 10));
        GridBagConstraints gbc_lblAttachments = new GridBagConstraints();
        gbc_lblAttachments.anchor = GridBagConstraints.WEST;
        gbc_lblAttachments.insets = new Insets(0, 0, 0, 5);
        gbc_lblAttachments.gridx = 0;
        gbc_lblAttachments.gridy = 0;
        atachementsPanel.add(lblAttachments, gbc_lblAttachments);

        JXLabel listOfAttachments = new JXLabel();
        listOfAttachments.setText("No attachments");
        listOfAttachments.setLineWrap(true);
        listOfAttachments.setBorder(new EmptyBorder(5, 0, 0, 0));
        GridBagConstraints gbc_listOfAttachments = new GridBagConstraints();
        gbc_listOfAttachments.fill = GridBagConstraints.HORIZONTAL;
        gbc_listOfAttachments.gridx = 1;
        gbc_listOfAttachments.gridy = 0;
        atachementsPanel.add(listOfAttachments, gbc_listOfAttachments);

        drawGeneralDetailsForEntity(entityModel);
        atachementsPanel.setVisible(hasAttachment);
    }

    private void drawGeneralDetailsForEntity(EntityModel entityModel) {
        headerPanel.setPhaseDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PHASE)));
        this.descriptionDetails.setText(getDescriptionForEntityModel(entityModel));
    }

    public void setEntityNameClickHandler(Runnable runnable){
        headerPanel.setActionToEntityLink(runnable);
    }

    public void setRefreshButton(AnAction refreshButton) {
        headerPanel.setRefreshButton(refreshButton);
    }
    public void setSaveSelectedPhaseButton(AnAction saveSelectedPhaseAction){
        headerPanel.setSaveSelectedPhaseButton(saveSelectedPhaseAction);
    }
    public void removeSaveSelectedPhaseButton(){
        headerPanel.removeSaveSelectedPhaseButton();
    }
    public  EntityModel getSelectedTransition(){
        return headerPanel.getSelectedTransition();
    }

    private String getDescriptionForEntityModel(EntityModel entityModel) {
        Document descriptionDoc = Jsoup.parse(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DESCRIPTION)));
        descriptionDoc.outputSettings().escapeMode(Entities.EscapeMode.base);
        descriptionDoc.outputSettings().charset(CharEncoding.US_ASCII);
        descriptionDoc.outputSettings().prettyPrint(false);
        return (null == descriptionDoc.text()) ? " " : descriptionDoc.text();
    }

    private JXPanel drawSpecificDetailsForEntity(EntityModel entityModel) {
        JXPanel ret = null;
        EntityIconFactory entityIconFactory = new EntityIconFactory(26, 26, 12);
        switch (Entity.getEntityType(entityModel)) {
            case DEFECT:
                headerPanel.setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(DEFECT)));
                hasAttachment = false;
                ret = updateUiWithDefectDetails(entityModel);
                break;
            case GHERKIN_TEST:
                headerPanel.setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(GHERKIN_TEST)));
                hasAttachment = false;
                ret = updateUiWithTestsDetails(entityModel, true);
                break;
            case MANUAL_TEST:
                headerPanel.setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(MANUAL_TEST)));
                hasAttachment = false;
                ret = updateUiWithTestsDetails(entityModel, false);
                break;
            case USER_STORY:
                headerPanel.setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(USER_STORY)));
                hasAttachment = false;
                ret = updateUiWithUserStoryDetails(entityModel);
                break;
            case TASK:
                headerPanel.setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(TASK)));
                ret = updateUiWithTaskDetails(entityModel);
                hasAttachment = false;
                break;
            case TEST_SUITE_RUN:
                headerPanel.setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(TEST_SUITE_RUN)));
                ret = updateUiWithTestSuiteRunDetails(entityModel);
                hasAttachment = false;
                break;
            case MANUAL_TEST_RUN:
                headerPanel.setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(MANUAL_TEST_RUN)));
                ret = updateUiWithManualTestRunDetails(entityModel);
                hasAttachment = false;
                break;
        }
        return ret;
    }
    private JXPanel updateUiWithTestSuiteRunDetails(EntityModel entityModel) {
        SuiteTestRunDetailsPanel suiteTestRunDetailsPanel = new SuiteTestRunDetailsPanel();
        suiteTestRunDetailsPanel.setSuiteTestNameDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_NAME)));
        suiteTestRunDetailsPanel.setDefaultRunByDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_RUN_BY)));
        suiteTestRunDetailsPanel.setStartedTimeDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_STARTED_TIME)));
        suiteTestRunDetailsPanel.setContentDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_CONTENT)));
        suiteTestRunDetailsPanel.setReleaseDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_RELEASE)));

        suiteTestRunDetailsPanel.setNativeStatusDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_NATIVE_STATUS)));
        suiteTestRunDetailsPanel.setAuthorDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_AUTHOR)));
        suiteTestRunDetailsPanel.setDraftRunDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_DRAFT_RUN)));
        suiteTestRunDetailsPanel.setLastModifiedDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_LAST_MODIFIED)));
        suiteTestRunDetailsPanel.setEnvironmentDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ENVIROMENT)));
        return suiteTestRunDetailsPanel;
    }
    private JXPanel updateUiWithManualTestRunDetails(EntityModel entityModel) {
        ManualTestRunDetailsPanel manualTestRunDetailsPanel = new ManualTestRunDetailsPanel();
        manualTestRunDetailsPanel.setTestNameDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_NAME)));
        manualTestRunDetailsPanel.setRunByDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_RUN_BY)));
        manualTestRunDetailsPanel.setStartedTimeDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_STARTED_TIME)));
        manualTestRunDetailsPanel.setContentDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_CONTENT)));
        manualTestRunDetailsPanel.setReleaseDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_RELEASE)));
        manualTestRunDetailsPanel.setEnvironmentDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ENVIROMENT)));

        manualTestRunDetailsPanel.setNativeStatusDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_NATIVE_STATUS)));
        manualTestRunDetailsPanel.setAuthorDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_AUTHOR)));
        manualTestRunDetailsPanel.setDurationDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_DURATION)));
        manualTestRunDetailsPanel.setDraftRunDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_DRAFT_RUN)));
        manualTestRunDetailsPanel.setVersionFromReleaseDescription(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_RUN_VERSION)));
        manualTestRunDetailsPanel.setLastModifiedDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_LAST_MODIFIED)));
        return manualTestRunDetailsPanel;
    }

    private JXPanel updateUiWithTestsDetails(EntityModel entityModel, boolean isGherkin) {
        TestDetailsPanel testDetailsPanel = new TestDetailsPanel();
        testDetailsPanel.setApplicationModulesDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_APPMODULE)));
        testDetailsPanel.setDesignerDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DESIGNER)));
        testDetailsPanel.setTestTypeDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_TYPE)));
        testDetailsPanel.setTestToolTypeDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TESTING_TOOL_TYPE)));
        testDetailsPanel.setCreatedDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_CREATED)));
        testDetailsPanel.setOwnerDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_OWNER)));
        testDetailsPanel.setEstimatedDurationDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ESTIMATED_DURATTION)));
        testDetailsPanel.setLastModifiedDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_LAST_MODIFIED)));
        testDetailsPanel.setCoveredContentDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_COVERED_CONTENT)));
        if (isGherkin) {
            testDetailsPanel.setAutomationStatusDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_AUTOMATION_STATUS)));
        }
        return testDetailsPanel;
    }

    private JXPanel updateUiWithDefectDetails(EntityModel entityModel) {
        DefectsDetailsPanel defectsDetailsPanel = new DefectsDetailsPanel();
        defectsDetailsPanel.setFeatureDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_FEATURE)));
        defectsDetailsPanel.setSeverityDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_SEVERITY)));
        defectsDetailsPanel.setSprintDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_SPRINT)));
        defectsDetailsPanel.setStoryPointsDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_STORYPOINTS)));
        defectsDetailsPanel.setBlockedDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_BLOCKED)));
        defectsDetailsPanel.setEnviromentDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ENVIROMENT)));
        defectsDetailsPanel.setReleaseDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_RELEASE)));
        defectsDetailsPanel.setDefectTypeDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DEFECT_TYPE)));
        defectsDetailsPanel.setLastModifiedDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_LAST_MODIFIED)));

        defectsDetailsPanel.setOwnerDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_OWNER)));
        defectsDetailsPanel.setDetectedByDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DETECTEDBY)));
        defectsDetailsPanel.setTeamDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEAM)));
        defectsDetailsPanel.setPriorityDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PRIORITY)));
        defectsDetailsPanel.setBlockedReasonDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_BLOCKED_REASON)));
        defectsDetailsPanel.setAppModuleDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_APPMODULE)));
        defectsDetailsPanel.setDetectedInReleaseDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DETECTEDINRELEASE)));
        defectsDetailsPanel.setCreationTimeDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_CREATION_TIME)));
        defectsDetailsPanel.setClosedOnDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_CLOSED_ON)));
        return defectsDetailsPanel;
    }

    private JXPanel updateUiWithTaskDetails(EntityModel entityModel) {
        TaskDetailsPanel taskDetailsPanel = new TaskDetailsPanel();
        taskDetailsPanel.setStoryDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_STORY)));
        taskDetailsPanel.setAuthorDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_AUTHOR)));
        taskDetailsPanel.setOwnerDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_OWNER)));
        taskDetailsPanel.setCreationTimeDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_CREATION_TIME)));
        taskDetailsPanel.setLastModifiedDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_LAST_MODIFIED)));

        taskDetailsPanel.setTaskTypeDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TASK_TYPE)));
        taskDetailsPanel.setRemainingHoursDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_REMAINING_HOURS)));
        taskDetailsPanel.setEstimatedHoursDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ESTIMATED_HOURS)));
        taskDetailsPanel.setInvestedHoursDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_INVESTED_HOURS)));
        return taskDetailsPanel;
    }

    private JXPanel updateUiWithUserStoryDetails(EntityModel entityModel) {
        UserStoryDetailsPanel userStoryDetailsPanel = new UserStoryDetailsPanel();
        userStoryDetailsPanel.setOwnerDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_OWNER)));
        userStoryDetailsPanel.setFeatureDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_FEATURE)));
        userStoryDetailsPanel.setSprintDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_SPRINT)));
        userStoryDetailsPanel.setStoryPointsDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_STORYPOINTS)));
        userStoryDetailsPanel.setBlockedDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_BLOCKED)));
        userStoryDetailsPanel.setLastRunsDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_LAST_RUNS)));
        userStoryDetailsPanel.setLastModifiedDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_LAST_MODIFIED)));

        userStoryDetailsPanel.setTeamDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEAM)));
        userStoryDetailsPanel.setAuthorDetailsDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_AUTHOR), "full_name"));
        userStoryDetailsPanel.setAppModuleDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_APPMODULE)));
        userStoryDetailsPanel.setItemOriginDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ITEM_ORIGIN)));
        userStoryDetailsPanel.setBlockedReasonDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_BLOCKED_REASON)));
        userStoryDetailsPanel.setReleaseDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_RELEASE)));
        userStoryDetailsPanel.setCreationTimeDetails(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_CREATION_TIME)));
        return userStoryDetailsPanel;
    }

    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
        headerPanel.setPossiblePhasesForEntity(phasesList);
    }

    private static class GoToBrowser extends AbstractAction {
        private EntityModel entityModel;
        private Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
        private ConnectionSettings connectionSettings;

        public GoToBrowser(String entityName, EntityModel entityModel, ConnectionSettings connectionSettings) {
            this.entityModel = entityModel;
            super.putValue(Action.NAME, entityName);
            super.putValue(Action.SHORT_DESCRIPTION, "Sample Action Description");
            this.connectionSettings = connectionSettings;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            URI uri =
                    UrlParser.createEntityWebURI(connectionSettings, Entity.getEntityType(entityModel), Integer.valueOf(UiUtil.getUiDataFromModel(entityModel.getValue("id"))));
            try {
                desktop.browse(uri);
            } catch (IOException e1) {
                e1.printStackTrace();
            }
        }
    }
}


