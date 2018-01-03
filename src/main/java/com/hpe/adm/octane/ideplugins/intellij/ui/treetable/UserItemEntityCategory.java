/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
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
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.mywork.MyWorkUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class UserItemEntityCategory implements EntityCategory {

    private String name = "";
    private List<Entity> entityTypes = new ArrayList<>();

    public UserItemEntityCategory(String name, Entity... entityTypes) {
        this.name = name;
        this.entityTypes = Arrays.asList(entityTypes);
    }

    @Override
    public boolean isInCategory(EntityModel userItem) {
        EntityModel entityModel = MyWorkUtil.getEntityModelFromUserItem(userItem);
        return entityTypes.contains(Entity.getEntityType(entityModel));
    }

    public List<Entity> getEntityTypes() {
        return entityTypes;
    }

    @Override
    public void setEntityTypes(List<Entity> entityTypes) {
        this.entityTypes = entityTypes;
    }

    public String getName() {
        return name;
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }
}