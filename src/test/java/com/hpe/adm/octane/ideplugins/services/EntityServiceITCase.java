package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.integrationtests.IntegrationTestBase;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.junit.Test;

public class EntityServiceITCase extends IntegrationTestBase{

    @Inject
    private EntityService entityService;

    @Test
    //@Ignore
    //TODO
    public void getSomeData() {
        System.out.println(entityService.findEntities(Entity.STORY).size());
        System.out.println(entityService.findEntities(Entity.DEFECT).size());
        System.out.println(entityService.findEntities(Entity.TEST).size());
    }


}
