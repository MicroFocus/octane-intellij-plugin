package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.integrationtests.IntegrationTestBase;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * This is just an example for now
 */
public class DependencyInjectionItCase extends IntegrationTestBase {

    @Inject
    private ConnectionSettings connectionSettings;

    @Inject
    private TestService testService;

    @Before
    public void before(){

    }

    @Test
    public void test(){
        //Test DI
        assertNotNull(connectionSettings);
        assertEquals(connectionSettings.getUserName(), null);
        assertNotNull(testService);
    }

    @After
    public void after(){

    }

}
