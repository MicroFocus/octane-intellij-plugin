package com.hpe.adm.octane.ideplugins;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.integrationtests.IntegrationTestBase;
import com.hpe.adm.octane.ideplugins.integrationtests.util.ConfigurationUtil;
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
    public void test(){
        //Test DI
        assertNotNull(connectionSettings);
        assertNotNull(testService);

        //This should is loaded in the TestModule from the config file
        assertEquals(connectionSettings.getUserName(), ConfigurationUtil.getString(ConfigurationUtil.PropertyKeys.USERNAME));
    }

}
