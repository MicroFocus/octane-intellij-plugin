package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.integrationtests.IntegrationTestBase;
import org.junit.Assert;
import org.junit.Ignore;
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

}
