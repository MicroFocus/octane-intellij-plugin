package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.google.protobuf.ServiceException;
import com.hpe.adm.nga.sdk.EntityList;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.filtering.Filter;

import java.util.*;


public class EntityService extends ServiceBase {

    @Inject
    ConnectionSettingsProvider connectionSettingsProvider;

    public Collection<EntityModel> getMyWork() {

        Entity[] myWorkEntities = new Entity[]{
                Entity.DEFECT, Entity.USER_STORY, Entity.TEST, Entity.TASK
        };

        Query.QueryBuilder currentUserQuery = new Query.QueryBuilder("owner", Query::equalTo,
                new Query.QueryBuilder("id", Query::equalTo, getCurrentUserId()));

        Collection<EntityModel> result = new ArrayList<>();

        Arrays.asList(myWorkEntities).forEach(entityType ->
                result.addAll(findEntities(entityType, currentUserQuery))
        );

        return result;
    }

    /**
     * All filters have && between them, used for simple filtering
     * @param entity
     * @param filters
     * @return
     */
    public Collection<EntityModel> findEntities(Entity entity, Filter... filters){
        EntityList entityList = getOctane().entityList(entity.getApiEntityName());

        Query.QueryBuilder queryBuilder = null;

        if(entity.isSubtype()){
            queryBuilder = entity.createMatchSubtypeQueryBuilder();
        }

        for(Filter filter : filters){
            if(queryBuilder == null) {
                queryBuilder = filter.createQueryBuilder();
            } else {
                queryBuilder = queryBuilder.and(filter.createQueryBuilder());
            }
        }

        if(queryBuilder != null){
            return entityList.get().query(queryBuilder.build()).execute();
        } else {
            return entityList.get().execute();
        }
    }

    public Collection<EntityModel> findEntities(Entity entity, Query.QueryBuilder query) {
        EntityList entityList = getOctane().entityList(entity.getApiEntityName());

        Query.QueryBuilder queryBuilder = null;

        if (entity.isSubtype()) {
            queryBuilder = entity.createMatchSubtypeQueryBuilder();
        }

        if (query != null) {
            if (queryBuilder == null) {
                queryBuilder = query;
            } else {
                queryBuilder = queryBuilder.and(query);
            }
        }

        if (queryBuilder != null) {
            return entityList.get().query(queryBuilder.build()).execute();
        } else {
            return entityList.get().execute();
        }
    }

    /**
     * TODO: this method is really inefficient for subtypes
     * Can return multiple entity types
     * @param entityFilters
     * @return
     */
    public Collection<EntityModel> findEntities(Map<Entity, List<Filter>> entityFilters){
        Collection<EntityModel> result = new ArrayList<>();

        for(Entity entity : entityFilters.keySet()){
            result.addAll(findEntities(entity, entityFilters.get(entity).toArray(new Filter[]{})));
        }

        return result;
    }
    /**
     * 
     * @param entityType
     * @param entityId
     * @return
     * @throws ServiceException
     */
    public EntityModel findEntity(Entity entityType, Long entityId) throws ServiceException {
        EntityModel result = null;
        try {
            result = getOctane().entityList(entityType.getApiEntityName()).at(entityId.intValue()).get().execute();
        } catch (Exception e) {
            throw new ServiceException("Failed to get the entity with id = '" + entityId + "' and type '" + entityType + "' ", e);
        }

        return result;
    }
}
