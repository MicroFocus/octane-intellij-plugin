package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.util.ui.tree.AbstractTreeModel;

import javax.swing.tree.TreePath;
import java.util.*;

public class EntityTreeModel extends AbstractTreeModel {

    private static final String ROOT = "root";
    private TreeMap<EntityCategory, List<EntityModel>> groupedEntities = new TreeMap<>();

    public EntityTreeModel() {
        init();
    }

    public EntityTreeModel(Collection<EntityModel> entityModels) {
        init();
        setEntities(entityModels);
    }

    private void init() {
        groupedEntities.put(EntityCategory.BACKLOG, new ArrayList<>());
        groupedEntities.put(EntityCategory.TASK, new ArrayList<>());
        groupedEntities.put(EntityCategory.TEST, new ArrayList<>());
        groupedEntities.put(EntityCategory.TEST_RUNS, new ArrayList<>());
        groupedEntities.put(EntityCategory.COMMENTS, new ArrayList<>());
    }

    private void clear() {
        groupedEntities.get(EntityCategory.BACKLOG).clear();
        groupedEntities.get(EntityCategory.TASK).clear();
        groupedEntities.get(EntityCategory.TEST).clear();
        groupedEntities.get(EntityCategory.TEST_RUNS).clear();
        groupedEntities.get(EntityCategory.COMMENTS).clear();
    }

    public void setEntities(Collection<EntityModel> entityModels) {
        clear();

        for (EntityModel entityModel : entityModels) {
            EntityCategory category = EntityCategory.getCategory(entityModel);

            if (category != null) {
                groupedEntities.get(category).add(entityModel);
            }
        }
    }

    public int size() {
        int size = 0;
        for (List<EntityModel> entityModels : groupedEntities.values()) {
            size += entityModels.size();
        }
        return size;
    }

    @Override
    public Object getRoot() {
        return ROOT;
    }

    @Override
    public Object getChild(Object parent, int index) {

        if (parent.equals(getRoot())) {
            //TreeMap keeps the keys sorted in the same manner all the time
            return groupedEntities.keySet().toArray(new EntityCategory[]{})[index];
        } else if (parent instanceof EntityCategory) {
            return groupedEntities.get(parent).get(index);
        }

        //Entities cannot have children with this type of model
        return null;
    }

    @Override
    public int getChildCount(Object parent) {
        if (parent.equals(getRoot())) {
            return groupedEntities.keySet().size();
        } else if (parent instanceof EntityCategory) {
            return groupedEntities.get(parent).size();
        }

        //Entities cannot have children with this type of model
        return 0;
    }

    @Override
    public boolean isLeaf(Object node) {
        if (node.equals(getRoot())) {
            return false;
        } else if (node instanceof EntityCategory) {
            return groupedEntities.get(node) == null || groupedEntities.get(node).size() == 0;
        }
        return true;
    }

    @Override
    public void valueForPathChanged(TreePath path, Object newValue) {
        //not needed
    }

    @Override
    public int getIndexOfChild(Object parent, Object child) {
        if (parent.equals(getRoot())) {
            List<EntityCategory> categories = new ArrayList<>(groupedEntities.keySet());
            return categories.indexOf(child);
        } else if (parent instanceof EntityCategory) {
            return groupedEntities.get(parent).indexOf(child);
        }

        //root
        return 0;
    }

    public enum EntityCategory {
        BACKLOG("Backlog", Entity.USER_STORY, Entity.DEFECT),
        TASK("Tasks", Entity.TASK),
        TEST("Tests", Entity.GHERKIN_TEST, Entity.MANUAL_TEST),
        COMMENTS("Mention in comments", Entity.COMMENT),
        TEST_RUNS("Runs", Entity.MANUAL_TEST_RUN , Entity.TEST_SUITE_RUN);

        private String name;
        private List<Entity> entityTypes = new ArrayList<>();

        EntityCategory(String name, Entity... entityTypes) {
            this.name = name;
            this.entityTypes = Arrays.asList(entityTypes);
        }

        public static EntityCategory getCategory(EntityModel entityModel) {
            for (EntityCategory category : EntityCategory.values()) {
                if (category.getEntityTypes().contains(Entity.getEntityType(entityModel))) {
                    return category;
                }
            }
            return null;
        }

        public List<Entity> getEntityTypes() {
            return entityTypes;
        }

        public String getName() {
            return name;
        }
    }

}
