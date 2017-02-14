package com.hpe.adm.octane.services;


import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.Lists;
import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.EntityListService;
import com.hpe.adm.nga.sdk.Octane;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import com.hpe.adm.octane.services.connection.OctaneProvider;
import com.hpe.adm.octane.services.exception.ServiceRuntimeException;
import com.hpe.adm.octane.services.filtering.Entity;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Comments only seem to work for work_item and tests (composite types)
 */
public class CommentService {

    @Inject
    private OctaneProvider octaneProvider;

    @Inject
    private UserService userService;

    //Aggregate types include their subtypes when the check is done
    private static final BiMap<Entity, String> supportedEntities = HashBiMap.create();
    static {
        supportedEntities.put(Entity.WORK_ITEM, "owner_work_item");
        supportedEntities.put(Entity.TEST, "owner_test");
    }

    private String getCommentReferenceFieldName(Entity entityType){
        if(supportedEntities.keySet().contains(entityType)){
            return supportedEntities.get(entityType);
        }
        if(entityType.isSubtype() && supportedEntities.keySet().contains(entityType.getSubtypeOf())){
            return supportedEntities.get(entityType.getSubtypeOf());
        }
        return null;
    }


    public Collection<EntityModel> getComments(EntityModel entityModel){
        Entity entityType = Entity.getEntityType(entityModel);
        String id = entityModel.getValue("id").getValue().toString();
        return getComments(entityType, id);
    }

    public Collection<EntityModel> getComments(Entity entityType, String id){
        //Check if comments are supported

        String referenceFieldName = getCommentReferenceFieldName(entityType);
        if(referenceFieldName!=null){
            return getComments(referenceFieldName, id);
        }

        //TODO: atoth: probably better to check features using the sdk metadata service
        throw new ServiceRuntimeException("Comments not supported for: " + entityType);
    }

    private Collection<EntityModel> getComments(String referenceFieldName, String id){
        Octane octane = octaneProvider.getOctane();

        EntityListService.Get get = octane.entityList(Entity.COMMENT.getApiEntityName()).get();

        Query query = new Query.QueryBuilder(referenceFieldName, Query::equalTo,
                new Query.QueryBuilder("id", Query::equalTo, id)).build();

        return get.query(query)
                .addOrderBy("creation_time",false)
                .execute();
    }

    /**
     * Add comment to entity
     * @param entityType
     * @param id
     * @param text should be html
     */
    public void postComment(Entity entityType, String id, String text){
        Octane octane = octaneProvider.getOctane();
        String referenceFieldName = getCommentReferenceFieldName(entityType);

        if(referenceFieldName == null){
            throw new ServiceRuntimeException("Comments not supported for: " + entityType);
        }

        //Create comment entity
        Set<FieldModel> fields = new HashSet<>();

        fields.add(new ReferenceFieldModel("author", userService.getCurrentUser()));
        fields.add(new ReferenceFieldModel(referenceFieldName, createOwner(entityType, id)));
        fields.add(new StringFieldModel("text", text));
        EntityModel newComment = new EntityModel(fields);

        octane.entityList(
                Entity.COMMENT.getApiEntityName())
                .create()
                .entities(Lists.asList(newComment, new EntityModel[]{}))
                .execute();
    }

    public void postComment(EntityModel entityModel, String text){
        Entity entityType = Entity.getEntityType(entityModel);
        String id = entityModel.getValue("id").getValue().toString();
        postComment(entityType, id, text);
    }

    public EntityModel deleteComment(String commentId){
        Integer commentIdInt = Integer.valueOf(commentId);
        Octane octane = octaneProvider.getOctane();

        return octane.entityList(Entity.COMMENT.getApiEntityName())
                .at(commentIdInt)
                .delete()
                .execute();
    }

    private EntityModel createOwner(Entity entityType, String id){
        //Create owner entity (needs id and type)
        Set<FieldModel> ownerFields = new HashSet<>();
        ownerFields.add(new StringFieldModel("id", id));
        String apiEntityType = supportedEntities.inverse().get(getCommentReferenceFieldName(entityType)).getTypeName();
        ownerFields.add(new StringFieldModel("type", apiEntityType));
        EntityModel owner = new EntityModel(ownerFields);
        return owner;
    }

}
