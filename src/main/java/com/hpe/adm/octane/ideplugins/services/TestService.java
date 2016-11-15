package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.NGA;
import com.hpe.adm.nga.sdk.authorisation.BasicAuthorisation;
import com.hpe.adm.nga.sdk.model.EntityModel;

import java.util.Collection;

public class TestService {

    private NGA nga;
    @Inject
    private ConnectionSettings connectionSettings;
    //Just for testing
    public TestService() {
        NGA.Builder builder = new NGA
                .Builder(new BasicAuthorisation(connectionSettings.getUserName(), connectionSettings.getPassword()))
                .Server(connectionSettings.getBaseUrl())
                .sharedSpace(connectionSettings.getSharedSpaceId())
                .workSpace(connectionSettings.getWorkspaceId());

        nga = builder.build();
    }

    public Collection<EntityModel> getDefects(){
        return nga.entityList("defects").get().execute();
    }

}
