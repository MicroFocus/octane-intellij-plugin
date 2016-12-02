package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.MultiReferenceFieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import org.apache.commons.lang.CharEncoding;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Entities;

import javax.swing.*;
import java.util.List;
import java.util.StringJoiner;

public class EntityDetailView implements View {

    private static final String DEFECT = "defect";
    private static final String TEST = "test";
    private static final String USER_STORY = "work_item";
    private static final String TASK = "task";

    private JPanel entityDetailsPanel;

	public EntityDetailView() {

    }

    @Override
    public JComponent getComponent() {

        return new JScrollPane(entityDetailsPanel);
    }

    //TODO: @osavencu: build it more generic (after it works)
    public void setEntityModel(EntityModel entityModel) {
        String entityType = entityModel.getValue("type").getValue().toString();
        switch (entityType) {
            case DEFECT:
                setEntityModelForDefects(entityModel);
                break;
            case TEST:
                setEntityModelForTests(entityModel);
                break;
            case USER_STORY:
                setEntityModelForUserStory(entityModel);
                break;
            case TASK:
                setEntityModelForTasks(entityModel);
                break;
        }

    }


    private void setEntityModelForDefects(EntityModel entityModel) {
        DefectsDetailsPanel defectsDetailsPanel = new DefectsDetailsPanel();
        defectsDetailsPanel.setTxtfldDescription(getDescriptionForEntityModel(entityModel));
        defectsDetailsPanel.setLblName(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_NAME)));
        defectsDetailsPanel.setTxtfldFeature(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_FEATURE)));
        defectsDetailsPanel.setTxtfldQaOwner(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_QAOWNER)));
        defectsDetailsPanel.setTxtfldSeverity(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_SEVERITY)));
        defectsDetailsPanel.setTxtfldTeam(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEAM)));
        defectsDetailsPanel.setTextfldStoryPoints(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_STORYPOINTS)));
        defectsDetailsPanel.setTextfldPriority(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PRIORITY)));
        defectsDetailsPanel.setComboBoxPhase(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PHASE)));

        defectsDetailsPanel.setTxtfldRelease(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_RELEASE)));
        defectsDetailsPanel.setTxtfldSprint(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_SPRINT)));

        defectsDetailsPanel.setTxtfldDetectedBy(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DETECTEDBY)));
        defectsDetailsPanel.setTxtfldBlocked(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_BLOCKED)));
        defectsDetailsPanel.setTxtfldBlockedReason(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_BLOCKEDBY)));
//		defectsDetailsPanel.setTxtfldGroup(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_GROUP)));
        defectsDetailsPanel.setTxtfldFeedbackType(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_FEEDBACKTYPE)));
//		defectsDetailsPanel.setTxtfldEnviroment(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ENVIROMENT)));
        defectsDetailsPanel.setTxtfldAppModules(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_APPMODULE)));

        defectsDetailsPanel.setTextfldDetectedInPush(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DETECTEDINPPUSH)));
        defectsDetailsPanel.setTxtfldDetectedInRelease(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DETECTEDINRELEASE)));
        defectsDetailsPanel.setTextfldFixedInPush(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_FIXEDINPUSH)));
        entityDetailsPanel = defectsDetailsPanel;
    }

    private void setEntityModelForTests(EntityModel entityModel) {
        TestDetailsPanel testDetailsPanel = new TestDetailsPanel();
        testDetailsPanel.setTxtfldDescription(getDescriptionForEntityModel(entityModel));
        testDetailsPanel.setLblName(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_NAME)));
        testDetailsPanel.setTxtfldAppModules(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_APPMODULE)));
        testDetailsPanel.setTtxtfldDesigner(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DESIGNER)));
        testDetailsPanel.setTxtfldManual(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_IS_MANUAL)));
        testDetailsPanel.setTextRunInReleases(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_RUNS_IN_RELEASES)));
        testDetailsPanel.setTxtfldTestType(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEST_SUBTYPE)));
        testDetailsPanel.setTxtfldTestingToolType(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TESTING_TOOL_TYPE)));
        testDetailsPanel.setTxtfldLastRuns(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_LAST_RUNS)));
        testDetailsPanel.setComboBoxPhase(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PHASE)));
        testDetailsPanel.setTxtfldCreated(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_CREATED)));
        testDetailsPanel.setTxtfldOwner(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_OWNER)));
        testDetailsPanel.setTxtfldFeature(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_FEATURE)));
        testDetailsPanel.setTxtfldEstimetedDuration(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ESTIMATED_DURATTION)));
        testDetailsPanel.setTxtfldRequirementCoverage(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_REQUIREMENT_COVERAGE)));
        testDetailsPanel.setTxtfldRunSet(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_RUN_SET)));
        testDetailsPanel.setTxtfldContent(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_CONTENT)));
        testDetailsPanel.setTxtFldCoveredContent(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_COVERED_CONTENT)));
        entityDetailsPanel = testDetailsPanel;
    }

    private void setEntityModelForUserStory(EntityModel entityModel) {

        UserStoryDetailsPanel userStoryDetailsPanel = new UserStoryDetailsPanel();
        userStoryDetailsPanel.setTxtfldDescription(getDescriptionForEntityModel(entityModel));
        userStoryDetailsPanel.setLblName(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_NAME)));
        userStoryDetailsPanel.setTxtfldTeam(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TEAM)));
        userStoryDetailsPanel.setTxtfldQaowner(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_QAOWNER_UDF)));
        userStoryDetailsPanel.setTxtfldAuthor(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_AUTHOR)));
        userStoryDetailsPanel.setComboBoxPhase(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PHASE)));
        userStoryDetailsPanel.setTxtfldRelease(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_RELEASE)));
        userStoryDetailsPanel.setTxtfldStoryPoints(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_STORYPOINTS)));
        userStoryDetailsPanel.setTxtfldDevstorypointsdays(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DEV_STORY_POINTS)));

        userStoryDetailsPanel.setTxtfldGroup(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_GROUP)));
        userStoryDetailsPanel.setTxtfldOwner(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_OWNER)));
        userStoryDetailsPanel.setTxtfldFeature(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_FEATURE)));
        userStoryDetailsPanel.setTxtfldSprint(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_SPRINT)));
        userStoryDetailsPanel.setTxtfldQastorypointsdays(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_QA_STORY_POINTS)));
        entityDetailsPanel = userStoryDetailsPanel;
    }

    private void setEntityModelForTasks(EntityModel entityModel) {
        TaskDetailsPanel taskDetailsPanel = new TaskDetailsPanel();
        taskDetailsPanel.setTxtfldDescription(getDescriptionForEntityModel(entityModel));
        taskDetailsPanel.setLblName(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_NAME)));
        taskDetailsPanel.setTxtfldStory(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_STORY)));
        taskDetailsPanel.setTxtfldAuthor(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_AUTHOR)));
        taskDetailsPanel.setTxtfldEstimatedHours(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_ESTIMATED_HOURS)));
        taskDetailsPanel.setTextInvestedHours(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_INVESTED_HOURS)));
        taskDetailsPanel.setTxtfldRemainingHours(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_REMAINING_HOURS)));

        taskDetailsPanel.setTxtfldOwner(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_OWNER)));
        taskDetailsPanel.setTxtfldType(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_TASK_TYPE)));
        taskDetailsPanel.setTxtfldCreationTime(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_CREATION_TIME)));
        taskDetailsPanel.setTxtfldLastModified(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_LAST_MODIFIED)));
        taskDetailsPanel.setComboBoxPhase(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_PHASE)));
        entityDetailsPanel = taskDetailsPanel;

    }

    private String getDescriptionForEntityModel(EntityModel entityModel) {
        Document descriptionDoc = Jsoup.parse(getUiDataFromModel(entityModel.getValue(DetailsViewDefaultFields.FIELD_DESCRIPTION)));
        descriptionDoc.outputSettings().escapeMode(Entities.EscapeMode.base);
        descriptionDoc.outputSettings().charset(CharEncoding.US_ASCII);
        descriptionDoc.outputSettings().prettyPrint(false);
        return descriptionDoc.text();
    }

    private String getUiDataFromModel(FieldModel fieldModel) {
        String result = "";
        if (null != fieldModel) {
            FieldModel tempFieldModel = null;
            if (fieldModel instanceof ReferenceFieldModel) {
                tempFieldModel = getValueOfChild((EntityModel) fieldModel.getValue(), "name");
                if (null != tempFieldModel) {
                    result = String.valueOf(tempFieldModel.getValue());
                }
            } else if (fieldModel instanceof MultiReferenceFieldModel) {
                result = getValueOfChildren((List<EntityModel>) fieldModel.getValue(), "name");
            } else {
                result = String.valueOf(fieldModel.getValue());
            }
        }
        return (null == result) ? " " : result;
    }

    private FieldModel getValueOfChild(EntityModel entityModel, String child) {
        FieldModel result = null;
        if (null != entityModel) {
            for (FieldModel fieldModel : entityModel.getValues()) {
                if (child.equals(fieldModel.getName())) {
                    result = fieldModel;
                }
            }
        }
        return result;
    }

    private String getValueOfChildren(List<EntityModel> entityModelList, String child) {
        StringJoiner result = new StringJoiner("; ");
        String tempFieldModelValue = " ";
        if (null != entityModelList) {
            for (EntityModel entityModel : entityModelList) {
                for (FieldModel fieldModel : entityModel.getValues()) {
                    if (child.equals(fieldModel.getName())) {
                        tempFieldModelValue = String.valueOf(fieldModel.getValue());
                    }
                }
                result.add(tempFieldModelValue);
            }
        }
        return result.toString();
    }



}
