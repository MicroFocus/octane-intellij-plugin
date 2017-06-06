/*
 * Copyright 2017 Hewlett-Packard Enterprise Development Company, L.P.
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