/*
 * © 2018 EntIT Software LLC, a Micro Focus company, L.P.
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

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.intellij.util.ui.tree.AbstractTreeModel;

import javax.swing.tree.TreePath;
import java.util.*;
import java.util.stream.Collectors;

public class EntityTreeModel extends AbstractTreeModel {

    private static final String ROOT = "root";
    private Map<EntityCategory, List<EntityModel>> groupedEntities = new LinkedHashMap<>();

    //Describes the top level
    private List<EntityCategory> entityCategories = new ArrayList<>();

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

        emptyCategories.forEach(emptyCategory -> groupedEntities.remove(emptyCategory));
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



    public List<EntityCategory> getEntityCategories() {
        return entityCategories;
    }

    public void setEntityCategories(List<EntityCategory> entityCategories) {
        this.entityCategories = entityCategories;
    }

    public Map<EntityCategory, List<EntityModel>> getGroupedEntities() {
        return groupedEntities;
    }
}
