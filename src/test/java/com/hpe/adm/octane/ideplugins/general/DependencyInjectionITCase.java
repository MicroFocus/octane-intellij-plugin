package com.hpe.adm.octane.ideplugins.general;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.integrationtests.IntegrationTestBase;
import com.hpe.adm.octane.ideplugins.services.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.TestService;
import org.junit.Ignore;
import org.junit.Test;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;

/**
 * This is just an example for now
 */
public class DependencyInjectionITCase extends IntegrationTestBase {

    @Inject
    private ConnectionSettings connectionSettings;

    @Inject
    private TestService testService;

    @Test
    @Ignore
    public void test(){
        //Test DI
        assertNotNull(connectionSettings);
        assertEquals(connectionSettings.getUserName(), "sa@nga");
        assertNotNull(testService);
    }

}
