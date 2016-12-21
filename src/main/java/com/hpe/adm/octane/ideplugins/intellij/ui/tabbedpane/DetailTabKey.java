package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.hpe.adm.octane.ideplugins.intellij.ui.util.EntityTypeIdPair;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.json.JSONObject;

class DetailTabKey extends EntityTypeIdPair {

    String entityName;

    public DetailTabKey(Long entityId, String entityName, Entity entityType){
        super(entityId, entityType);
        this.entityName = entityName;
    }

    public String getEntityName() {
        return entityName;
    }

    public void setEntityName(String entityName) {
        this.entityName = entityName;
    }

    public static JSONObject toJsonObject(DetailTabKey detailTabKey){
        JSONObject jsonObject = EntityTypeIdPair.toJsonObject(detailTabKey);
        jsonObject.put("entityName", detailTabKey.getEntityName());
        return jsonObject;
    }

    public static DetailTabKey fromJsonObject(JSONObject jsonObject){
        EntityTypeIdPair entityTypeIdPair = EntityTypeIdPair.fromJsonObject(jsonObject);
        String entityName = jsonObject.getString("entityName");

        DetailTabKey detailTabKey = new DetailTabKey(
                entityTypeIdPair.getEntityId(),
                entityName,
                entityTypeIdPair.getEntityType());

        return detailTabKey;
    }
}