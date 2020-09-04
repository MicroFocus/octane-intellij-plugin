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

package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.mywork.MyWorkUtil;
import com.hpe.adm.octane.ideplugins.services.util.EntityTypeIdPair;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ui.JBColor;
import com.intellij.util.ui.UIUtil;
import org.apache.commons.lang.StringUtils;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;
import java.util.*;

import static com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields.*;
import static com.hpe.adm.octane.ideplugins.services.util.Util.getContainerItemForCommentModel;
import static com.hpe.adm.octane.ideplugins.services.util.Util.getUiDataFromModel;

public class EntityTreeCellRenderer implements TreeCellRenderer {

    private static final Map<Entity, HashSet<String>> entityFields = new HashMap<>();
    private static final String[] commonFields = new String[]{"id", "name", "phase"};
    private static final String[] progressFields = new String[]{FIELD_INVESTED_HOURS, FIELD_REMAINING_HOURS, FIELD_ESTIMATED_HOURS};
    private static final Map<String, String> subtypeNames = new HashMap<>();
    static {
        subtypeNames.put(Entity.USER_STORY.getSubtypeName(), "User Story");
        subtypeNames.put(Entity.DEFECT.getSubtypeName(), "Defect");
        subtypeNames.put(Entity.QUALITY_STORY.getSubtypeName(), "Quality Story");
        subtypeNames.put(Entity.EPIC.getSubtypeName(), "Epic");
        subtypeNames.put(Entity.FEATURE.getSubtypeName(), "Feature");
        subtypeNames.put(Entity.GHERKIN_TEST.getSubtypeName(), "Gherkin Test");
        subtypeNames.put(Entity.MANUAL_TEST.getSubtypeName(), "Manual Test");
        subtypeNames.put(Entity.MANUAL_TEST_RUN.getSubtypeName(), "Manual Run");
        subtypeNames.put(Entity.AUTOMATED_TEST_RUN.getSubtypeName(), "Automated Run");
        subtypeNames.put(Entity.TEST_SUITE.getSubtypeName(), "Test Suite");
        subtypeNames.put(Entity.TEST_SUITE_RUN.getSubtypeName(), "Run Suite");
        subtypeNames.put(Entity.REQUIREMENT.getSubtypeName(), "Requirement");
        subtypeNames.put(Entity.BDD_SCENARIO.getSubtypeName(), "BDD Scenario");
    }

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    @Inject
    private EntityIconFactory entityIconFactory;

    static {
        //US
        entityFields.put(Entity.USER_STORY, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.USER_STORY), commonFields);
        entityFields.get(Entity.USER_STORY).add("subtype");
        entityFields.get(Entity.USER_STORY).add(FIELD_RELEASE);
        entityFields.get(Entity.USER_STORY).add(FIELD_AUTHOR);
        entityFields.get(Entity.USER_STORY).add(FIELD_STORYPOINTS);
        entityFields.get(Entity.USER_STORY).add(FIELD_OWNER);
        Collections.addAll(entityFields.get(Entity.USER_STORY), progressFields);

        entityFields.put(Entity.QUALITY_STORY, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.QUALITY_STORY), commonFields);
        entityFields.get(Entity.QUALITY_STORY).add("subtype");
        entityFields.get(Entity.QUALITY_STORY).add(FIELD_RELEASE);
        entityFields.get(Entity.QUALITY_STORY).add(FIELD_AUTHOR);
        entityFields.get(Entity.QUALITY_STORY).add(FIELD_STORYPOINTS);
        entityFields.get(Entity.QUALITY_STORY).add(FIELD_OWNER);
        Collections.addAll(entityFields.get(Entity.QUALITY_STORY), progressFields);

        //TASK
        entityFields.put(Entity.TASK, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.TASK), commonFields);
        //entityFields.get(Entity.TASK).add("type"); //not a subtype
        entityFields.get(Entity.TASK).add(FIELD_RELEASE);
        entityFields.get(Entity.TASK).add(FIELD_AUTHOR);
        entityFields.get(Entity.TASK).add(FIELD_STORY);
        entityFields.get(Entity.TASK).add(FIELD_OWNER);
        Collections.addAll(entityFields.get(Entity.TASK), progressFields);

        //DEFECT
        entityFields.put(Entity.DEFECT, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.DEFECT), commonFields);
        entityFields.get(Entity.DEFECT).add("subtype");
        entityFields.get(Entity.DEFECT).add(FIELD_ENVIROMENT);
        entityFields.get(Entity.DEFECT).add(FIELD_DETECTEDBY);
        entityFields.get(Entity.DEFECT).add(FIELD_STORYPOINTS);
        entityFields.get(Entity.DEFECT).add(FIELD_SEVERITY);
        entityFields.get(Entity.DEFECT).add(FIELD_OWNER);
        Collections.addAll(entityFields.get(Entity.DEFECT), progressFields);

        //GHERKIN_TEST
        entityFields.put(Entity.GHERKIN_TEST, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.GHERKIN_TEST), commonFields);
        entityFields.get(Entity.GHERKIN_TEST).add("subtype");
        entityFields.get(Entity.GHERKIN_TEST).add(FIELD_TEST_TYPE);
        entityFields.get(Entity.GHERKIN_TEST).add(FIELD_AUTHOR);
        entityFields.get(Entity.GHERKIN_TEST).add(FIELD_OWNER);
        entityFields.get(Entity.GHERKIN_TEST).add("automation_status");

        //MANUAL_TEST
        entityFields.put(Entity.MANUAL_TEST, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.MANUAL_TEST), commonFields);
        entityFields.get(Entity.MANUAL_TEST).add("subtype");
        entityFields.get(Entity.MANUAL_TEST).add(FIELD_TEST_TYPE);
        entityFields.get(Entity.MANUAL_TEST).add(FIELD_AUTHOR);
        entityFields.get(Entity.MANUAL_TEST).add(FIELD_OWNER);
        entityFields.get(Entity.MANUAL_TEST).add("steps_num");
        entityFields.get(Entity.MANUAL_TEST).add("automation_status");

        //MANUAL TEST RUNS
        entityFields.put(Entity.MANUAL_TEST_RUN, new HashSet<>());
        entityFields.get(Entity.MANUAL_TEST_RUN).add("subtype");
        entityFields.get(Entity.MANUAL_TEST_RUN).add("name");
        entityFields.get(Entity.MANUAL_TEST_RUN).add("native_status");
        entityFields.get(Entity.MANUAL_TEST_RUN).add(FIELD_AUTHOR);
        entityFields.get(Entity.MANUAL_TEST_RUN).add(FIELD_ENVIROMENT);
        entityFields.get(Entity.MANUAL_TEST_RUN).add("started");
        entityFields.get(Entity.MANUAL_TEST_RUN).add("test_name");

        //COMMENTS
        entityFields.put(Entity.COMMENT, new HashSet<>());
        entityFields.get(Entity.COMMENT).add("id");
        entityFields.get(Entity.COMMENT).add("text");
        entityFields.get(Entity.COMMENT).add("author");
        entityFields.get(Entity.COMMENT).add("owner_work_item");
        entityFields.get(Entity.COMMENT).add("owner_test");
        entityFields.get(Entity.COMMENT).add("owner_run");

        //REQUIREMENTS
        entityFields.put(Entity.REQUIREMENT, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.REQUIREMENT), commonFields);
        entityFields.get(Entity.REQUIREMENT).add(FIELD_AUTHOR);
        entityFields.get(Entity.REQUIREMENT).add(FIELD_OWNER);
        entityFields.get(Entity.REQUIREMENT).add(FIELD_RELEASE);
        entityFields.get(Entity.REQUIREMENT).add("subtype");

        //BDD SCENARIO
        entityFields.put(Entity.BDD_SCENARIO, new HashSet<>());
        entityFields.get(Entity.BDD_SCENARIO).add(FIELD_NAME);
        entityFields.get(Entity.BDD_SCENARIO).add(FIELD_OWNER);
        entityFields.get(Entity.BDD_SCENARIO).add(FIELD_AUTOMATION_STATUS);
    }

    /**
     * Returns a map of fields that this cell will display for each entity type, needed for making the request
     *
     * @return
     */
    public static Map<Entity, Set<String>> getEntityFieldMap() {
        return Collections.unmodifiableMap(entityFields);
    }

    public static String getSubtypeName(String subtype) {
        String subtypeName = subtypeNames.get(subtype);
        return subtypeName != null ? subtypeName : subtype;
    }

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus) {

        //Root
        if (value instanceof String) {

            return new JLabel((String) value);

        } else if (value instanceof EntityCategory) {

            EntityTreeModel model = (EntityTreeModel) tree.getModel();
            EntityCategory category = (EntityCategory) value;

            int count = model.getChildCount(value);

            String labelText = category.getName() + " (" + count + ")";
            JXLabel lbl = new JXLabel(labelText);

            Font font = new Font(lbl.getFont().getFontName(), Font.BOLD, lbl.getFont().getSize() + 4);
            lbl.setFont(font);

            if (selected && hasFocus && !UIUtil.isUnderDarcula()) {
                lbl.setForeground(JBColor.background());
            }

            return lbl;

        } else if (value instanceof EntityModel) {

            //Base data
            EntityModel userItem = (EntityModel) value;
            EntityModel entityModel = MyWorkUtil.getEntityModelFromUserItem(userItem);
            Entity entityType = Entity.getEntityType(entityModel);
            Long entityId = Long.valueOf(getUiDataFromModel(entityModel.getValue("id")));

            EntityModelRow rowPanel;

            if (selected && hasFocus) {
                rowPanel = new EntityModelRow(entityIconFactory, true);
            } else {
                rowPanel = new EntityModelRow(entityIconFactory, false);
            }

            rowPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, JBColor.border()));

            //Add owner if entity is not owned by the current plugin user
            String ownerText = null;

            //check who the entity belongs too if the owner field exits
            if (entityModel.getValue("owner") != null) {
                if (entityModel.getValue("owner").getValue() == null) {
                    ownerText = "";
                } else {
                    EntityModel ownerEntity = (EntityModel) entityModel.getValue("owner").getValue();
                    ownerText = getUiDataFromModel(ownerEntity.getValue(FIELD_FULL_NAME));
                }
            }
            if (ownerText != null) {
                rowPanel.addDetails(
                        "Owner",
                        ownerText,
                        DetailsPosition.TOP);
            }

            //Check if the item is dismissible or not
            if (MyWorkUtil.isUserItemDismissible(userItem)) {
                rowPanel.addSimpleDetails("Dismissible", DetailsPosition.BOTTOM);
            }

            //Check if the rendered item is the active item or not
            EntityTypeIdPair entityTypeIdPair =
                    EntityTypeIdPair.fromJsonObject(
                            idePluginPersistentState.loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM));

            if (new EntityTypeIdPair(entityId, entityType).equals(entityTypeIdPair)) {
                rowPanel.setIcon(Entity.getEntityType(entityModel), true);
            } else {
                rowPanel.setIcon(Entity.getEntityType(entityModel), false);
            }

            //Add specific details for each item type

            if (entityType != Entity.COMMENT) {
                if (entityType.equals(Entity.MANUAL_TEST_RUN) || entityType.equals(Entity.TEST_SUITE_RUN)) {
                    String nativeStatus = getUiDataFromModel(entityModel.getValue(FIELD_TEST_RUN_NATIVE_STATUS));
                    rowPanel.addDetails("Status", nativeStatus, DetailsPosition.TOP);
                } else {
                    String phase = getUiDataFromModel(entityModel.getValue("phase"));
                    rowPanel.addDetails("Phase", phase, DetailsPosition.TOP);
                }

                String id = wrapHtml("<b>" + entityId + "</b>");
                rowPanel.setEntityName(id, getUiDataFromModel(entityModel.getValue("name")));
            }

            if (Entity.DEFECT.equals(entityType)) {
                rowPanel.setEntitySubTitle(
                        getUiDataFromModel(entityModel.getValue(FIELD_ENVIROMENT)),
                        "No environment");

                addStoryPoints(rowPanel, entityModel);
                addRelationFieldDetails(rowPanel, entityModel, FIELD_DETECTEDBY, FIELD_FULL_NAME, "Detected by", DetailsPosition.TOP);
                rowPanel.addDetails("Severity", getUiDataFromModel(entityModel.getValue(FIELD_SEVERITY)), DetailsPosition.TOP);
                addProgress(rowPanel, entityModel);

            } else if (Entity.USER_STORY.equals(entityType) || Entity.QUALITY_STORY.equals(entityType)) {
                rowPanel.setEntitySubTitle(
                        getUiDataFromModel(entityModel.getValue(FIELD_RELEASE)),
                        "No release");

                addStoryPoints(rowPanel, entityModel);
                addRelationFieldDetails(rowPanel, entityModel, FIELD_AUTHOR, FIELD_FULL_NAME, "Author", DetailsPosition.TOP);
                addProgress(rowPanel, entityModel);

            } else if (Entity.TASK.equals(entityType)) {

                //Add parent details for tasks
                EntityModel storyEntityModel = (EntityModel) entityModel.getValue("story").getValue();

                String type;
                if (storyEntityModel.getValue("subtype") != null) {
                    type = storyEntityModel.getValue("subtype").getValue().toString();
                } else {
                    type = storyEntityModel.getValue("type").getValue().toString();
                }
                String storyTypeName = subtypeNames.get(type);

                StringBuilder parentInfoSb = new StringBuilder();
                parentInfoSb.append("<html>");
                parentInfoSb.append("Task of " + storyTypeName.toLowerCase());
                parentInfoSb.append(" <b>" + storyEntityModel.getValue("id").getValue().toString() + ":</b>");
                parentInfoSb.append(" " + storyEntityModel.getValue("name").getValue().toString());
                parentInfoSb.append("</html>");
                rowPanel.setEntitySubTitle(parentInfoSb.toString(), "no parent");

                addRelationFieldDetails(rowPanel, entityModel, FIELD_AUTHOR, FIELD_FULL_NAME, "Author", DetailsPosition.TOP);
                addProgress(rowPanel, entityModel);

            } else if (Entity.GHERKIN_TEST.equals(entityType)) {

                rowPanel.setEntitySubTitle(
                        getUiDataFromModel(entityModel.getValue(FIELD_TEST_TYPE)),
                        "");
                addRelationFieldDetails(rowPanel, entityModel, FIELD_AUTHOR, FIELD_FULL_NAME, "Author", DetailsPosition.TOP);
                rowPanel.addDetails("Automation status",
                        getUiDataFromModel(entityModel.getValue("automation_status")),
                        DetailsPosition.BOTTOM);

                //addProgress(rowPanel, entityModel);

            } else if (Entity.MANUAL_TEST.equals(entityType)) {

                rowPanel.setEntitySubTitle(
                        getUiDataFromModel(entityModel.getValue(FIELD_TEST_TYPE)),
                        "");

                addRelationFieldDetails(rowPanel, entityModel, FIELD_AUTHOR, FIELD_FULL_NAME, "Author", DetailsPosition.TOP);
                rowPanel.addDetails("Steps", getUiDataFromModel(entityModel.getValue("steps_num")), DetailsPosition.BOTTOM);
                String automationStatus = getUiDataFromModel(entityModel.getValue("automation_status"));
                if (StringUtils.isNotEmpty(automationStatus)) {
                    rowPanel.addDetails("Automation status", automationStatus, DetailsPosition.BOTTOM);
                }
            } else if (Entity.COMMENT.equals(entityType)) {

                String text = getUiDataFromModel(entityModel.getValue("text"));
                text = " Comment: " + Util.stripHtml(text);
                FieldModel owner = getContainerItemForCommentModel(entityModel);
                if (owner != null) {
                    String ownerId = getUiDataFromModel(owner, "id");
                    String ownerName = getUiDataFromModel(owner, "name");
                    String ownerType = getUiDataFromModel(owner, "type");

                    String entityName = wrapHtml("Comment on " + getSubtypeName(ownerType).toLowerCase() + ": " + "<b>" + ownerId + "</b>" + " " + ownerName);

                    rowPanel.setEntityName("", entityName);
                    rowPanel.setEntitySubTitle(text, "");
                    addRelationFieldDetails(rowPanel, entityModel, FIELD_AUTHOR, FIELD_FULL_NAME, "Author", DetailsPosition.TOP);
                } else {
                    rowPanel.setEntityName("N/A", "");
                }

            } else if (Entity.MANUAL_TEST_RUN.equals(entityType)) {
                rowPanel.setEntitySubTitle(
                        getUiDataFromModel(entityModel.getValue(FIELD_ENVIROMENT)),
                        "No environment");
                addRelationFieldDetails(rowPanel, entityModel, FIELD_AUTHOR, FIELD_FULL_NAME, "Author", DetailsPosition.TOP);
                rowPanel.addDetails("Started", getUiDataFromModel(entityModel.getValue(FIELD_TEST_RUN_STARTED_DATE)), DetailsPosition.BOTTOM);

            } else if (Entity.TEST_SUITE_RUN.equals(entityType)) {
                rowPanel.setEntitySubTitle(
                        getUiDataFromModel(entityModel.getValue(FIELD_ENVIROMENT)),
                        "No environment");
                addRelationFieldDetails(rowPanel, entityModel, FIELD_AUTHOR, FIELD_FULL_NAME, "Author", DetailsPosition.TOP);
                rowPanel.addDetails("Started", getUiDataFromModel(entityModel.getValue(FIELD_TEST_RUN_STARTED_DATE)), DetailsPosition.BOTTOM);
            }

            return rowPanel;
        }

        return new JLabel("N/A");
    }

    private void addProgress(EntityModelRow rowPanel, EntityModel entityModel) {
        rowPanel.addDetails("Invested hours", getUiDataFromModel(entityModel.getValue(FIELD_INVESTED_HOURS)), DetailsPosition.BOTTOM);
        rowPanel.addDetails("Remaining hours", getUiDataFromModel(entityModel.getValue(FIELD_REMAINING_HOURS)), DetailsPosition.BOTTOM);
        rowPanel.addDetails("Estimated hours", getUiDataFromModel(entityModel.getValue(FIELD_ESTIMATED_HOURS)), DetailsPosition.BOTTOM);
    }

    private void addStoryPoints(EntityModelRow entityModelRow, EntityModel entityModel) {
        String storyPoints = getUiDataFromModel(entityModel.getValue(FIELD_STORYPOINTS));
        entityModelRow.addDetails("SP", storyPoints, DetailsPosition.TOP);
    }

    /**
     * Add a relation entity model to the row
     *
     * @param entityModelRow     UI object
     * @param entityModel        parent entity
     * @param relationFieldName  field of the related entity
     * @param relationLabelValue field displayed from the related entity
     * @param displayLabel       label of the displayed field
     * @param displayPosition    position of the displayed field
     */
    private void addRelationFieldDetails(
            EntityModelRow entityModelRow,
            EntityModel entityModel,
            String relationFieldName,
            String relationLabelValue,
            String displayLabel,
            DetailsPosition displayPosition) {

        if (entityModel.getValue(relationFieldName) == null ||
                entityModel.getValue(relationFieldName).getValue() == null) {
            return;
        }

        EntityModel relatedEntity = (EntityModel) entityModel.getValue(relationFieldName).getValue();
        String storyPoints = getUiDataFromModel(relatedEntity.getValue(relationLabelValue));
        entityModelRow.addDetails(displayLabel, storyPoints, displayPosition);
    }

    private static String wrapHtml(String string) {
        return "<html><body>" + string + "</body></html>";
    }

}