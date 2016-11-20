package com.hpe.adm.octane.ideplugins.intellij.ui.views.treetable;

import com.hpe.adm.nga.sdk.model.EntityModel;
import org.jdesktop.swingx.treetable.AbstractTreeTableModel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeMap;


public class EntityTreeTableModel extends AbstractTreeTableModel {

    private final static String[] COLUMN_NAMES = {"group", "id", "name"};

    private TreeMap<String, List<EntityModel>> groupedEntities;

    /**
     * Currently only supports one group by clause
     * @param entityModels
     * @param groupByFieldName can be null
     */
    public EntityTreeTableModel(Collection<EntityModel> entityModels, String groupByFieldName) {
        super(new Object());

        //todo: maybe move this somewhere else
        groupedEntities = new TreeMap<>();

        for(EntityModel entityModel : entityModels){
            String fieldValue = entityModel.getValue(groupByFieldName).getValue().toString();
            if(groupedEntities.get(fieldValue) == null){
                groupedEntities.put(fieldValue, new ArrayList<>());
            }
            groupedEntities.get(fieldValue).add(entityModel);
        }

    }

    public int getColumnCount() {
        return COLUMN_NAMES.length;
    }

    @Override
    public String getColumnName(int column) {
        return COLUMN_NAMES[column];
    }
    
    @Override
    public boolean isCellEditable(Object node, int column) {
        return false;
    }

    @Override
    public boolean isLeaf(Object node) {
        return node instanceof EntityModel;
    }

    public int getChildCount(Object parent) {
        if (parent instanceof String) {
            return groupedEntities.get(parent).size();
        }
        return groupedEntities.keySet().size();
    }

    public Object getChild(Object parent, int index) {
        if (parent instanceof String) {
            return groupedEntities.get(parent).get(index);
        }
        return groupedEntities.keySet().toArray(new String[]{})[index];
    }

    public int getIndexOfChild(Object parent, Object child) {
        String groupColumn = (String) parent;
        List<EntityModel> entities = groupedEntities.get(groupColumn);
        return entities.indexOf(child);
    }

    public Object getValueAt(Object node, int column) {
        if (node instanceof String) {
            String groupFieldValue = (String) node;
            switch (column) {
                case 0:
                    return groupFieldValue;
                case 1:
                    return "";
                case 2:
                    return "";
            }
        } else if (node instanceof EntityModel) {

            if(column == 0){
                return "";
            } else {
                EntityModel entity = (EntityModel) node;
                String fieldName = COLUMN_NAMES[column];
                return entity.getValue(fieldName).getValue().toString();
            }
        }
        return null;
    }

    public void setValueAt(Object value, Object node, int column) {
        //inline edition is not supported, so I'll just leave this here
    }
}
