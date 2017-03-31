package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.services.filtering.Entity;

import java.util.List;

/**
 * Provide a way to make a named grouping for entity types
 */
public interface EntityCategory {

    static EntityCategory getCategory(EntityModel entityModel, List<EntityCategory> entityCategories) {
        for(EntityCategory category : entityCategories){
            if(category.isInCategory(entityModel)){
                return category;
            }
        }
        return null;
    }

    boolean isInCategory(EntityModel entityModel);

    List<Entity> getEntityTypes();

    void setEntityTypes(List<Entity> entityTypes);

    String getName();

    void setName(String name);

}