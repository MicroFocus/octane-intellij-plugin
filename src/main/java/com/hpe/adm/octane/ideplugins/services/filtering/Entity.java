package com.hpe.adm.octane.ideplugins.services.filtering;

import com.hpe.adm.nga.sdk.Query;

/**
 * Constants for sdk query builder use
 */
public enum Entity {
    DEFECT("defects"),
    WORK_ITEM("work_items"),
    STORY(Entity.WORK_ITEM, "story"),
    TEST(Entity.WORK_ITEM, "test");

    //This is the name of the entity passed to the sdk, used for the rest, call, usually plural
    private String apiEntityName;

    //This is a marker that the entity is a subtype of another entity, (probably work_item),
    private Entity subtypeOf;

    //In case this is a subtype, you need to give the value of the subtype field to filter on
    private String subtypeFieldValue;

    Entity(String apiEntityName){
        this.apiEntityName = apiEntityName;
    }

    Entity(Entity subtypeOf, String subtypeFieldValue){
        this.subtypeOf = subtypeOf;
        this.apiEntityName = subtypeOf.getApiEntityName();
        this.subtypeFieldValue = subtypeFieldValue;
    }

    public boolean isSubtype(){
        return subtypeOf != null;
    }

    public Entity getSubtypeOf() {
        return subtypeOf;
    }

    public String getSubtypeFieldValue() {
        return subtypeFieldValue;
    }

    public String getApiEntityName() {
        return apiEntityName;
    }

    public Query.QueryBuilder createMatchSubtypeQueryBuilder(){
        if(isSubtype()){
           return new Query.QueryBuilder("subtype", Comparator.EQ.getFunction(), getSubtypeFieldValue());
        }
        throw new RuntimeException("Entity " + apiEntityName + "is not a subtype");
    }

}
