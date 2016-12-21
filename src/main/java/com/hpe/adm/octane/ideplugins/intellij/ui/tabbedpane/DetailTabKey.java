package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.json.JSONObject;

class DetailTabKey {

    Long entityId;
    Entity entityType;
    String entityName;

    public DetailTabKey(Long entityId, String entityName, Entity entityType){
        this.entityId = entityId;
        this.entityType = entityType;
        this.entityName = entityName;
    }

    public Long getEntityId() {
        return entityId;
    }

    public Entity getEntityType() {
        return entityType;
    }

    public String getEntityName() {
        return entityName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DetailTabKey that = (DetailTabKey) o;

        if (entityId != null ? !entityId.equals(that.entityId) : that.entityId != null) return false;
        return entityType == that.entityType;

    }

    @Override
    public int hashCode() {
        int result = entityId != null ? entityId.hashCode() : 0;
        result = 31 * result + (entityType != null ? entityType.hashCode() : 0);
        return result;
    }

    public static JSONObject toJsonObject(DetailTabKey detailTabKey){
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("id", detailTabKey.getEntityId());
        jsonObject.put("entityType", detailTabKey.getEntityType().name());
        jsonObject.put("entityName", detailTabKey.getEntityName());
        return jsonObject;
    }

    public static DetailTabKey fromJsonObject(JSONObject jsonObject){
        Long entityId = jsonObject.getLong("id");
        String entityName = jsonObject.getString("entityName");
        Entity entityType = Entity.valueOf(jsonObject.getString("entityType"));
        return new DetailTabKey(entityId, entityName, entityType);
    }
}