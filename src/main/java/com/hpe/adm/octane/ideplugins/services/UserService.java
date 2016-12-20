package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.EntityList;
import com.hpe.adm.nga.sdk.Octane;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.connection.OctaneProvider;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceRuntimeException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

import java.util.Collection;

public class UserService {

    @Inject
    private OctaneProvider octaneProvider;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    /**
     * Well this is horrible, this method is needed because cross filtering work item owner by name does not work
     * @return
     */
    public Long getCurrentUserId(){

        Octane octane = octaneProvider.getOctane();
        String currentUser = connectionSettingsProvider.getConnectionSettings().getUserName();

        EntityList entityList = octane.entityList(Entity.WORKSPACE_USER.getApiEntityName());
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



}
