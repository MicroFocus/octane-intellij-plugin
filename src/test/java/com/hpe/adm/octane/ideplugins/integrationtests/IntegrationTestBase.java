package com.hpe.adm.octane.ideplugins.integrationtests;

import com.google.inject.Guice;
import com.google.inject.Injector;
import org.junit.Before;

/**
 * Enables the use of the {@link com.google.inject.Inject} annotation
 */
public abstract class IntegrationTestBase {

    private Injector injector = Guice.createInjector(new TestModule());

    @Before
    public void setup () {
        injector.injectMembers(this);
    }

}
