package com.hpe.adm.octane.ideplugins.services.filtering;

import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.EntityModel;

/**
 * Constants for sdk query builder use
 */
public enum Entity {

    DEFECT("defects", "defect"),

    WORK_ITEM("work_items", "work_item"),
    STORY(Entity.WORK_ITEM, "story"),

    TEST("tests", "test"),
    MANUAL_TEST(Entity.TEST, "test_manual"),
    GHERKIN_TEST(Entity.TEST, "test_gherkin"),

    TASK("tasks", "task");

    //This is the name of the entity passed to the sdk, used for the rest, call, usually plural
    private String apiEntityName;

    //This is the type name that is returned with every entity
    private String typeName;

    //This is a marker that the entity is a subtype of another entity, (probably work_item),
    private Entity subtypeOf;

    //In case this is a subtype, you need to give the value of the subtype field to filter on
    private String subtypeFieldValue;

    Entity(String apiEntityName, String typeName){
        this.apiEntityName = apiEntityName;
        this.typeName = typeName;
    }

    Entity(Entity subtypeOf, String subtypeName){
        this.subtypeOf = subtypeOf;
        this.apiEntityName = subtypeOf.getApiEntityName();
        this.subtypeFieldValue = subtypeName;
    }

    public boolean isSubtype(){
        return subtypeOf != null;
    }

    public Entity getSubtypeOf() {
        return subtypeOf;
    }

    public String getSubtypeName() {
        return subtypeFieldValue;
    }

    public String getTypeName() {
        if(isSubtype()) {
            return subtypeOf.getTypeName();
        } else {
            return typeName;
        }
    }

    public String getApiEntityName() {
        return apiEntityName;
    }

    public Query.QueryBuilder createMatchSubtypeQueryBuilder(){
        if(isSubtype()){
           return new Query.QueryBuilder("subtype", Comparator.EQ.getFunction(), getSubtypeName());
        }
        throw new RuntimeException("Entity " + apiEntityName + "is not a subtype");
    }


    public static Entity getEntityType(EntityModel entityModel){

        if(entityModel.getValue("subtype") != null){
            String subtype = entityModel.getValue("subtype").getValue().toString();

            //try finding the subtype
            if(subtype != null) {
                for (Entity entity : Entity.values()) {
                    if (entity.isSubtype() && entity.getSubtypeName().equals(subtype)) {
                        return entity;
                    }
                }
            }
        }

        if(entityModel.getValue("type") != null){
            String type = entityModel.getValue("type").getValue().toString();

            //try finding the subtype
            if(type != null) {
                for (Entity entity : Entity.values()) {
                    if (!entity.isSubtype() && entity.getTypeName().equals(type)) {
                        return entity;
                    }
                }
            }
        }

        return null;
    }

}
