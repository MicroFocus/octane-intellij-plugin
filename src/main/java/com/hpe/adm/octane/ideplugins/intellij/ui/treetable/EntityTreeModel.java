package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.util.ui.tree.AbstractTreeModel;

import javax.swing.tree.TreePath;
import java.util.*;
import java.util.stream.Collectors;

public class EntityTreeModel extends AbstractTreeModel {

    private static final String ROOT = "root";
    private Map<EntityCategory, List<EntityModel>> groupedEntities = new LinkedHashMap<>();

    //Describes the top level
    private List<EntityCategory> entityCategories = getDefaultEntityCategories();

    public EntityTreeModel() {}

    public EntityTreeModel(Collection<EntityModel> entityModels) {
        setEntities(entityModels);
    }

    public EntityTreeModel(List<EntityCategory> entityCategories, Collection<EntityModel> entityModels) {
        this.entityCategories = entityCategories;
        setEntities(entityModels);
    }

    public void setEntities(Collection<EntityModel> entityModels) {
        //init to preserve category order
        entityCategories.forEach(entityCategory -> groupedEntities.put(entityCategory, new ArrayList<>()));

        for (EntityModel entityModel : entityModels) {
            EntityCategory category = EntityCategory.getCategory(entityModel, entityCategories);
            if (category != null) {
                groupedEntities.get(category).add(entityModel);
            }
        }

        //Remove empty categories
        List<EntityCategory> emptyCategories =
                    entityCategories
                    .stream()
                    .filter(entityCategory -> groupedEntities.get(entityCategory).size()==0)
                    .collect(Collectors.toList());

        groupedEntities.remove(emptyCategories);
    }

    public static List<EntityCategory> getDefaultEntityCategories(){
        List<EntityCategory> entityCategories = new ArrayList<>();
        entityCategories.add(new EntityCategory("Backlog", Entity.USER_STORY, Entity.DEFECT, Entity.EPIC, Entity.FEATURE));
        entityCategories.add(new EntityCategory("Tasks", Entity.TASK));
        entityCategories.add(new EntityCategory("Tests", Entity.GHERKIN_TEST, Entity.MANUAL_TEST));
        entityCategories.add(new EntityCategory("Mention in comments", Entity.COMMENT));
        entityCategories.add(new EntityCategory("Runs", Entity.MANUAL_TEST_RUN , Entity.TEST_SUITE_RUN));
        return entityCategories;
    }

    public int size() {
        return groupedEntities.values()
                .stream()
                .mapToInt(list -> list.size())
                .sum();
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

    public static class EntityCategory {

        private String name = "";
        private List<Entity> entityTypes = new ArrayList<>();

        public EntityCategory(String name, Entity... entityTypes) {
            this.name = name;
            this.entityTypes = Arrays.asList(entityTypes);
        }

        public static EntityCategory getCategory(EntityModel entity, List<EntityCategory> entityCategories) {
            for (EntityCategory category : entityCategories) {
                if (category.getEntityTypes().contains(Entity.getEntityType(entity))) {
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

    public List<EntityCategory> getEntityCategories() {
        return entityCategories;
    }

    public void setEntityCategories(List<EntityCategory> entityCategories) {
        this.entityCategories = entityCategories;
    }
}
