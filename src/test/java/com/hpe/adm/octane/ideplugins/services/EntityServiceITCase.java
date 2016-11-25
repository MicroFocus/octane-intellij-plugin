package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.integrationtests.IntegrationTestBase;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.junit.Assert;
import org.junit.Test;

public class EntityServiceITCase extends IntegrationTestBase{

    @Inject
    private EntityService entityService;

    @Inject
    private TestService testService;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    @Test
    //@Ignore
    //TODO
    public void getSomeData() {
        System.out.println(entityService.findEntities(Entity.STORY).size());
        System.out.println(entityService.findEntities(Entity.DEFECT).size());
        System.out.println(entityService.findEntities(Entity.TEST).size());
    }

    @Test
    @Inject
    public void testConnection(){
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();

        try{
            testService.getOctane(connectionSettings);
        } catch (Exception e){
            Assert.fail(e.toString());
        }

        try{
            testService.getOctane(connectionSettings);
        } catch (Exception e){
            Assert.fail(e.toString());
        }

    }


}
