package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.services.filtering.Entity;
import com.hpe.adm.octane.services.mywork.MyWorkUtil;

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