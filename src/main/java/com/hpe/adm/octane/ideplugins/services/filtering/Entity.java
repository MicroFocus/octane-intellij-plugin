package com.hpe.adm.octane.ideplugins.services.filtering;

import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.EntityModel;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Constants for sdk query builder use
 */
public enum Entity {

    WORK_ITEM("work_items", "work_item"),
    USER_STORY(Entity.WORK_ITEM, "story"),
    QUALITY_STORY(Entity.WORK_ITEM, "quality_story"),
    DEFECT(Entity.WORK_ITEM, "defect"),
    WORK_ITEM_ROOT(Entity.WORK_ITEM, "work_item_root"),
    EPIC(Entity.WORK_ITEM, "epic"),
    FEATURE(Entity.WORK_ITEM, "feature"),

    TEST("tests", "test"),
    MANUAL_TEST(Entity.TEST, "test_manual"),
    GHERKIN_TEST(Entity.TEST, "gherkin_test"),

    TASK("tasks", "task"),
    PHASE("phases", "phases"),
    TRANSITION("transitions", "transition"),
    TEST_RUN("runs", "run"),
    MANUAL_TEST_RUN(TEST_RUN,"run_manual"),
    TEST_SUITE_RUN(TEST_RUN,"run_suite"),

    COMMENT("comments", "comment"),

    RUN("runs", "run"),

    WORKSPACE_USER("workspace_users", "workspace_user");


    //This is the name of the entity passed to the sdk, used for the rest, call, usually plural
    private String apiEntityName;

    //This is the type name that is returned with every entity
    private String typeName;

    //This is a marker that the entity is a subtype of another entity, (probably work_item),
    private Entity subtypeOf;

    //In case this is a subtype, you need to give the value of the subtype field to filter on
    private String subtypeFieldValue;

    Entity(String apiEntityName, String typeName) {
        this.apiEntityName = apiEntityName;
        this.typeName = typeName;
    }

    Entity(Entity subtypeOf, String subtypeName) {
        this.subtypeOf = subtypeOf;
        this.apiEntityName = subtypeOf.getApiEntityName();
        this.subtypeFieldValue = subtypeName;
    }

    public static Entity getEntityType(EntityModel entityModel) {

        if (entityModel.getValue("subtype") != null) {
            String subtype = entityModel.getValue("subtype").getValue().toString();

            //try finding the subtype
            if (subtype != null) {
                for (Entity entity : Entity.values()) {
                    if (entity.isSubtype() && entity.getSubtypeName().equals(subtype)) {
                        return entity;
                    }
                }
            }
        }

        if (entityModel.getValue("type") != null) {
            String type = entityModel.getValue("type").getValue().toString();

            //try finding the subtype
            if (type != null) {
                for (Entity entity : Entity.values()) {
                    if (!entity.isSubtype() && entity.getTypeName().equals(type)) {
                        return entity;
                    }
                }
            }
        }

        return null;
    }

    public static Set<Entity> getSubtypes(Entity entity){
        if(entity.isSubtype()){
            return Collections.emptySet();
        } else {
            Set<Entity> result = new HashSet<>();
            for(Entity subType : Entity.values()){
                if(entity.equals(subType.getSubtypeOf())){
                    result.add(subType);
                }
            }
            return result;
        }
    }

    public boolean isSubtype() {
        return subtypeOf != null;
    }

    public Entity getSubtypeOf() {
        return subtypeOf;
    }

    public String getSubtypeName() {
        return subtypeFieldValue;
    }

    public String getTypeName() {
        if (isSubtype()) {
            return subtypeOf.getTypeName();
        } else {
            return typeName;
        }
    }

    public String getApiEntityName() {
        return apiEntityName;
    }

    public Query.QueryBuilder createMatchSubtypeQueryBuilder() {
        if (isSubtype()) {
            return new Query.QueryBuilder("subtype", Comparator.EQ.getFunction(), getSubtypeName());
        }
        throw new RuntimeException("Entity " + apiEntityName + "is not a subtype");
    }

}