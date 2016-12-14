package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.ui.JBColor;
import org.apache.commons.lang.StringUtils;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;
import java.util.*;

import static com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields.*;

class EntityTreeCellRenderer implements TreeCellRenderer {

    private static final Map<Entity, HashSet<String>> entityFields = new HashMap<>();
    private static final String[] commonFields = new String[]{"id", "name", "phase"};
    private static final String[] progressFields = new String[]{FIELD_INVESTED_HOURS, FIELD_REMAINING_HOURS, FIELD_ESTIMATED_HOURS};

    static {
        //US
        entityFields.put(Entity.USER_STORY, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.USER_STORY), commonFields);
        entityFields.get(Entity.USER_STORY).add("subtype");
        entityFields.get(Entity.USER_STORY).add(FIELD_RELEASE);
        entityFields.get(Entity.USER_STORY).add(FIELD_AUTHOR);
        entityFields.get(Entity.USER_STORY).add(FIELD_STORYPOINTS);
        Collections.addAll(entityFields.get(Entity.USER_STORY), progressFields);

        //TASK
        entityFields.put(Entity.TASK, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.TASK), commonFields);
        //entityFields.get(Entity.TASK).add("type"); //not a subtype
        entityFields.get(Entity.TASK).add(FIELD_RELEASE);
        entityFields.get(Entity.TASK).add(FIELD_AUTHOR);
        Collections.addAll(entityFields.get(Entity.TASK), progressFields);

        //DEFECT
        entityFields.put(Entity.DEFECT, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.DEFECT), commonFields);
        entityFields.get(Entity.DEFECT).add("subtype");
        entityFields.get(Entity.DEFECT).add(FIELD_ENVIROMENT);
        entityFields.get(Entity.DEFECT).add(FIELD_DETECTEDBY);
        entityFields.get(Entity.DEFECT).add(FIELD_STORYPOINTS);
        entityFields.get(Entity.DEFECT).add(FIELD_SEVERITY);
        Collections.addAll(entityFields.get(Entity.DEFECT), progressFields);

        //GHERKIN_TEST
        entityFields.put(Entity.GHERKIN_TEST, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.GHERKIN_TEST), commonFields);
        entityFields.get(Entity.GHERKIN_TEST).add("subtype");
        entityFields.get(Entity.GHERKIN_TEST).add(FIELD_TEST_TYPE);
        entityFields.get(Entity.GHERKIN_TEST).add(FIELD_AUTHOR);
        entityFields.get(Entity.GHERKIN_TEST).add("automation_status");

        //MANUAL_TEST
        entityFields.put(Entity.MANUAL_TEST, new HashSet<>());
        Collections.addAll(entityFields.get(Entity.MANUAL_TEST), commonFields);
        entityFields.get(Entity.MANUAL_TEST).add("subtype");
        entityFields.get(Entity.MANUAL_TEST).add(FIELD_TEST_TYPE);
        entityFields.get(Entity.MANUAL_TEST).add(FIELD_AUTHOR);
        entityFields.get(Entity.MANUAL_TEST).add("steps_num");
    }

    /**
     * Returns a map of fields that this cell will display for each entity type, needed for making the request
     *
     * @return
     */
    public static Map<Entity, Set<String>> getEntityFieldMap() {
        return Collections.unmodifiableMap(entityFields);
    }

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus) {

        //Root
        if (value instanceof String) {
            return new JLabel((String) value);
        } else if (value instanceof EntityTreeModel.EntityCategory) {

            EntityTreeModel model = (EntityTreeModel) tree.getModel();
            EntityTreeModel.EntityCategory category = (EntityTreeModel.EntityCategory) value;
            int count = model.getChildCount(value);
            String labelText = category.getName() + " (" + count + ")";

            JXLabel lbl = new JXLabel(labelText);
            Font font = new Font(lbl.getFont().getFontName(), Font.BOLD, lbl.getFont().getSize() + 4);
            lbl.setFont(font);
            if (selected && hasFocus) {
                lbl.setForeground(new Color(255, 255, 255));
            }

            return lbl;
        } else if (value instanceof EntityModel) {

            EntityModel entityModel = (EntityModel) value;
            Entity entityType = Entity.getEntityType(entityModel);

            EntityModelRow rowPanel;

            if (selected && hasFocus) {
                rowPanel = new EntityModelRow(new Color(255, 255, 255));
            } else {
                rowPanel = new EntityModelRow();
            }
            rowPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, JBColor.border()));
            rowPanel.setIcon(Entity.getEntityType(entityModel));

            rowPanel.setEntityName(
                    UiUtil.getUiDataFromModel(entityModel.getValue("id")),
                    UiUtil.getUiDataFromModel(entityModel.getValue("name")));

            String phase = "Phase: " + UiUtil.getUiDataFromModel(entityModel.getValue("phase"));
            rowPanel.addDetailsTop(phase);

            if (Entity.DEFECT.equals(entityType)) {
                rowPanel.setEntityDetails(
                        UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_ENVIROMENT)),
                        "No environment");

                rowPanel.addDetailsTop(getStoryPoints(entityModel));
                rowPanel.addDetailsTop("Detected by: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_DETECTEDBY)));
                rowPanel.addDetailsBottom("Severity: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_SEVERITY)));
                addProgress(rowPanel, entityModel);
            } else if (Entity.USER_STORY.equals(entityType)) {
                rowPanel.setEntityDetails(
                        UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_RELEASE)),
                        "No release");

                rowPanel.addDetailsTop(getStoryPoints(entityModel));
                rowPanel.addDetailsTop("Author: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_AUTHOR), FIELD_FULL_NAME));
                addProgress(rowPanel, entityModel);
            } else if (Entity.TASK.equals(entityType)) {
                rowPanel.setEntityDetails(
                        UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_RELEASE)),
                        "No release");

                rowPanel.addDetailsTop("Author: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_AUTHOR), FIELD_FULL_NAME));
                addProgress(rowPanel, entityModel);
            } else if (Entity.GHERKIN_TEST.equals(entityType)) {
                rowPanel.setEntityDetails(
                        UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_TEST_TYPE)),
                        "");

                rowPanel.addDetailsTop("Author: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_AUTHOR), FIELD_FULL_NAME));
                rowPanel.addDetailsBottom("Automation status: " + UiUtil.getUiDataFromModel(entityModel.getValue("automation_status")));

                //addProgress(rowPanel, entityModel);
            } else if (Entity.MANUAL_TEST.equals(entityType)) {
                rowPanel.setEntityDetails(
                        UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_TEST_TYPE)),
                        "");

                rowPanel.addDetailsTop("Author: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_AUTHOR), FIELD_FULL_NAME));
                rowPanel.addDetailsBottom("Steps: " + UiUtil.getUiDataFromModel(entityModel.getValue("steps_num")));
            }


            return rowPanel;
        }

        return new JLabel("N/A");
    }

    private void addProgress(EntityModelRow rowPanel, EntityModel entityModel) {
        rowPanel.addDetailsBottom("Invested hours: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_INVESTED_HOURS)));
        rowPanel.addDetailsBottom("Remaining hours: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_REMAINING_HOURS)));
        rowPanel.addDetailsBottom("Estimated hours: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_ESTIMATED_HOURS)));
    }

    private String getStoryPoints(EntityModel entityModel) {
        String storyPoints = UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_STORYPOINTS));
        if (StringUtils.isEmpty(storyPoints)) {
            storyPoints = "-";
        }
        return "SP: " + storyPoints;
    }

}