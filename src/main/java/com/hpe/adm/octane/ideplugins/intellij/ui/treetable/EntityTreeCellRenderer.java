package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.nga.sdk.model.EntityModel;

import javax.swing.*;
import javax.swing.border.LineBorder;
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
            
            return new JLabel(labelText);
        }
        
        else if (value instanceof EntityModel){
        	
            EntityModel entityModel = (EntityModel) value;
        	
        	//EntityModelRow rowWidget = new EntityModelRow();
        	
        	//rowWidget.setIcon(EntityIconFactory.getIcon(Entity.getEntityType(entityModel)));

            //assume it has a name at least
            String name = entityModel.getValue("name").getValue().toString();
            //rowWidget.setEntityName(name);

            JPanel panel = new JPanel(new BorderLayout(0,0));
            panel.setBorder(new LineBorder(Color.black, 1));
            panel.add(new JLabel(name));

          	return panel;
        }
        
        return new JLabel("N/A");
    }

}
