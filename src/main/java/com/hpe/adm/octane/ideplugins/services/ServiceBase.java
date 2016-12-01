package com.hpe.adm.octane.ideplugins.services;


import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.EntityList;
import com.hpe.adm.nga.sdk.Octane;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceRuntimeException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

import java.util.Collection;

public abstract class ServiceBase {

    private static ConnectionSettings previousConnectionSettings = new ConnectionSettings();
    private static Octane octane;
    private long currentUserId = -1;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    /**
     * Create an octane client with the current connection settings
     * Use this for all service methods that need an octane client
     * @return {@link Octane}
     */
    protected synchronized Octane getOctane() {

        ConnectionSettings currentConnectionSettings = connectionSettingsProvider.getConnectionSettings();

        if (!currentConnectionSettings.equals(previousConnectionSettings) || octane == null) {
            octane = new Octane
                    .Builder(new UserAuthorisation(currentConnectionSettings.getUserName(), currentConnectionSettings.getPassword()))
                    .Server(currentConnectionSettings.getBaseUrl())
                    .sharedSpace(currentConnectionSettings.getSharedSpaceId())
                    .workSpace(currentConnectionSettings.getWorkspaceId())
                    .build();

            currentUserId = getCurrentUserId(octane);
        }

        previousConnectionSettings = currentConnectionSettings;

        return octane;
    }

    /**
     * Well this is horrible, this method is needed because cross filtering work item owner by name does not work
     * TODO: this call req rights that an ordinary user might now have, need to fix the filtering or find another solution
     * @return
     */
    private Long getCurrentUserId(Octane octane){
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

    protected Long getCurrentUserId(){
        return currentUserId;
    }

}
