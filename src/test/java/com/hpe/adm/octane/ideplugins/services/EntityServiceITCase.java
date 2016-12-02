package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.integrationtests.IntegrationTestBase;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.junit.Assert;
import org.junit.Test;

import java.util.Collection;

public class EntityServiceITCase extends IntegrationTestBase{

    @Inject
    private EntityService entityService;

    @Inject
    private TestService testService;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    @Test
    public void testEntityConstants() {

        Collection<EntityModel> entities;
        entities = entityService.findEntities(Entity.MANUAL_TEST);

        if(entities.size()>0){
            EntityModel firstEntity = entities.iterator().next();
            Assert.assertEquals(Entity.getEntityType(firstEntity), Entity.MANUAL_TEST);
        }

        entities = entityService.findEntities(Entity.DEFECT);
        if(entities.size()>0){
            EntityModel firstEntity = entities.iterator().next();
            Assert.assertEquals(Entity.getEntityType(firstEntity), Entity.DEFECT);
        }

        entities = entityService.findEntities(Entity.STORY);
        if(entities.size()>0){
            EntityModel firstEntity = entities.iterator().next();
            Assert.assertEquals(Entity.getEntityType(firstEntity), Entity.STORY);
        }
    }

    @Test
    public void testGetMyWork(){
        Collection<EntityModel> entityModels = entityService.getMyWork();
        System.out.println(entityModels.size());
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
