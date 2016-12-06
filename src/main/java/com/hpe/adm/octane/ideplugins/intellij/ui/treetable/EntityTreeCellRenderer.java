package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.ui.JBColor;

import javax.swing.*;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;

public class EntityTreeCellRenderer implements TreeCellRenderer {

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

            EntityModelRow rowWidget;
            if(selected && hasFocus) {
                rowWidget = new EntityModelRow(new Color(255,255,255));
            } else {
                rowWidget = new EntityModelRow();
            }

        	rowWidget.setIcon(EntityIconFactory.getIcon(Entity.getEntityType(entityModel)));

            //assume it has a name at least
            String name = entityModel.getValue("name").getValue().toString();
            rowWidget.setEntityName(name);

            rowWidget.setBorder(BorderFactory.createLineBorder(JBColor.border()));

            return rowWidget;
        }
        
        return new JLabel("N/A");
    }

}
