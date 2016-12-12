package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.ui.JBColor;
import org.apache.commons.lang.StringUtils;

import javax.swing.*;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;

import static com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields.*;

class EntityTreeCellRenderer implements TreeCellRenderer {

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus) {
        
    	//Root
        if(value instanceof String){
            return new JLabel((String)value);
        }
        else if (value instanceof EntityTreeModel.EntityCategory){

            EntityTreeModel model = (EntityTreeModel) tree.getModel();
            EntityTreeModel.EntityCategory category = (EntityTreeModel.EntityCategory) value;
            int count = model.getChildCount(value);
            String labelText = category.getName() + " (" + count + ")";

            JLabel lbl = new JLabel(labelText);
            lbl.setPreferredSize(new Dimension(200,25));
            if(selected && hasFocus) {
                lbl.setForeground(new Color(255,255,255));
            }

            return lbl;
        }
        
        else if (value instanceof EntityModel){

            EntityModel entityModel = (EntityModel) value;
            Entity entityType = Entity.getEntityType(entityModel);

            EntityModelRow rowPanel;

            if(selected && hasFocus) {
                rowPanel = new EntityModelRow(new Color(255,255,255));
            } else {
                rowPanel = new EntityModelRow();
            }
            rowPanel.setBorder(BorderFactory.createMatteBorder(0,0,1,0,JBColor.border()));
        	rowPanel.setIcon(Entity.getEntityType(entityModel));

            //assume it has a name at least
            String name =
                    UiUtil.getUiDataFromModel(entityModel.getValue("id"))
                    + ": "
                    + UiUtil.getUiDataFromModel(entityModel.getValue("name"));

            rowPanel.setEntityName(name);

            String phase = "Phase: " + UiUtil.getUiDataFromModel(entityModel.getValue("phase"));
            rowPanel.addDetailsTop(phase);

            if(Entity.DEFECT.equals(entityType)){

                rowPanel.setEntityDetails(
                        UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_ENVIROMENT)),
                        "No environment");

                rowPanel.addDetailsTop("SP: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_STORYPOINTS)));
                rowPanel.addDetailsTop("Detected by: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_DETECTEDBY)));
                rowPanel.addDetailsBottom("Severity: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_SEVERITY)));
                addProgress(rowPanel, entityModel);
            }
            else if (Entity.USER_STORY.equals(entityType)){

                rowPanel.setEntityDetails(
                        UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_RELEASE)),
                        "No release");

                rowPanel.addDetailsTop(getStoryPoints(entityModel));
                rowPanel.addDetailsTop("Author: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_AUTHOR), FIELD_FULL_NAME));

                addProgress(rowPanel, entityModel);
            }
            else if (Entity.TASK.equals(entityType)){

                rowPanel.setEntityDetails(
                        UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_RELEASE)),
                        "No release");

                rowPanel.addDetailsTop(getStoryPoints(entityModel));
                rowPanel.addDetailsTop("Author: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_AUTHOR), FIELD_FULL_NAME));

                addProgress(rowPanel, entityModel);
            }
            else if (Entity.GHERKIN_TEST.equals(entityType)){

                rowPanel.setEntityDetails(
                        UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_TEST_TYPE)),
                        "");

                rowPanel.addDetailsTop("Author: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_AUTHOR), FIELD_FULL_NAME));
                rowPanel.addDetailsBottom("Automation status: " + UiUtil.getUiDataFromModel(entityModel.getValue("automation_status")));

                //addProgress(rowPanel, entityModel);
            }
            else if (Entity.MANUAL_TEST.equals(entityType)){

                rowPanel.setEntityDetails(
                        UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_TEST_TYPE)),
                        "");

                //rowPanel.addDetailsTop(UiUtil.getUiDataFromModel(entityModel.getValue());
                rowPanel.addDetailsTop("Author: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_AUTHOR), FIELD_FULL_NAME));

                rowPanel.addDetailsBottom("Steps: " + UiUtil.getUiDataFromModel(entityModel.getValue("steps_num")));

                //addProgress(rowPanel, entityModel);
            }


            return rowPanel;
        }
        
        return new JLabel("N/A");
    }

    private void addProgress(EntityModelRow rowPanel, EntityModel entityModel){
        rowPanel.addDetailsBottom("Invested hours: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_INVESTED_HOURS)));
        rowPanel.addDetailsBottom("Remaining hours: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_REMAINING_HOURS)));
        rowPanel.addDetailsBottom("Estimated hours: " + UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_ESTIMATED_HOURS)));
    }

    private String getStoryPoints(EntityModel entityModel){
        String storyPoints = UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_STORYPOINTS));
        if(StringUtils.isEmpty(storyPoints)){
            storyPoints = "-";
        }
        return "SP: " + storyPoints;
    }

}
