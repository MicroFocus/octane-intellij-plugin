package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.EntityList;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceRuntimeException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.filtering.Filter;

import java.util.*;

public class EntityService extends ServiceBase{

    @Inject
    ConnectionSettingsProvider connectionSettingsProvider;

    public Collection<EntityModel> getMyWork(){

        Entity[] myWorkEntities = new Entity[]{
          Entity.DEFECT, Entity.STORY, Entity.TEST, Entity.TASK
        };

        Query.QueryBuilder currentUserQuery = new Query.QueryBuilder("owner", Query::equalTo,
                new Query.QueryBuilder("id", Query::equalTo, getCurrentUserId()));

        Collection<EntityModel> result = new ArrayList<>();

        Arrays.asList(myWorkEntities).forEach( entityType ->
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

    /**
     * Well this is horrible, this method is needed because cross filtering work item owner by name does not work
     * TODO: this call req rights that an ordaniry user might now have, need to fix the filtering or find another solution
     * @return
     */
    private Long getCurrentUserId(){
        String currentUser = connectionSettingsProvider.getConnectionSettings().getUserName();
        EntityList entityList = getOctane().entityList(Entity.WORKSPACE_USER.getApiEntityName());
        Collection<EntityModel> entityModels =
                entityList.get().query(
                        new Query.QueryBuilder("name", Query::equalTo, currentUser).build()).addFields("id")
                        .execute();

        if(entityModels.size()!=1){
            throw new ServiceRuntimeException("Failed to retrieve logged in user id");
        } else {
            return Long.parseLong(entityModels.iterator().next().getValue("id").getValue().toString());
        }
    }

    public Collection<EntityModel> findEntities(Entity entity, Query.QueryBuilder query){
        EntityList entityList = getOctane().entityList(entity.getApiEntityName());

        Query.QueryBuilder queryBuilder = null;

        if(entity.isSubtype()){
            queryBuilder = entity.createMatchSubtypeQueryBuilder();
        }

        if(query!=null){
            if(queryBuilder == null) {
                queryBuilder = query;
            } else {
                queryBuilder = queryBuilder.and(query);
            }
        }

        if(queryBuilder != null){
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
}
