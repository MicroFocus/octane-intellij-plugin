package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.integrationtests.IntegrationTestBase;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.junit.Assert;
import org.junit.Test;

public class TestServiceITCase extends IntegrationTestBase{

    @Inject
    private TestService testService;

    @Test
    public void testConnectionTest(){
        try{
            testService.testConnection();
        } catch (Exception ex){
            Assert.fail(ex.toString());
        }
    }

    @Test
    //@Ignore
    //TODO
    public void getSomeData() {
        System.out.println(testService.findEntities(Entity.STORY).size());
        System.out.println(testService.findEntities(Entity.DEFECT).size());
        System.out.println(testService.findEntities(Entity.TEST).size());
    }


}
