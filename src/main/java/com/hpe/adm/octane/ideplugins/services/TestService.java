package com.hpe.adm.octane.ideplugins.services;

import com.hpe.adm.nga.sdk.EntityList;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.filtering.Filter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public class TestService extends ServiceBase{

    /**
     * All filters have && between them, used for simple filtering
     * @param entity
     * @param filters
     * @return
     */
    public Collection<EntityModel> findEntities(Entity entity, Filter... filters){
        EntityList entityList = getNGA().entityList(entity.getApiEntityName());

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
     * Check if the current connection settings are valid
     */
    public void testConnection() throws Exception {
        //Try to build an nga client(will check auth), also try to a query to see if the shared+workspace are valid
        try{
            findEntities(Entity.WORK_ITEM);
        } catch (Exception ex){
            throw new Exception(ex);
        }
    }


}
