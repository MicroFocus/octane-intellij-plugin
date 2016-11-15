package com.hpe.adm.octane.ideplugins.integrationtests;

import com.google.inject.Guice;
import com.google.inject.Injector;
import org.junit.Before;

public abstract class IntegrationTestBase {

    protected Injector injector = Guice.createInjector(new TestModule());

    @Before
    public void setup () {
        injector.injectMembers(this);
    }

}
