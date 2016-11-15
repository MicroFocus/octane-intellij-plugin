package com.hpe.adm.octane.ideplugins.integrationtests.util;

import org.apache.commons.configuration2.CombinedConfiguration;
import org.apache.commons.configuration2.SystemConfiguration;
import org.apache.commons.configuration2.builder.fluent.Configurations;
import org.apache.commons.configuration2.ex.ConfigurationException;

public class ConfigurationUtil {

    private static final String PROP_PREFIX = "sdk";
    public enum Properties {
        URL(PROP_PREFIX + ".url"),
        SHAREDSPACE(PROP_PREFIX + ".sharedSpaceId"),
        WORKSPACE(PROP_PREFIX + ".workspaceId"),
        USERNAME(PROP_PREFIX + ".username"),
        PASSWORD(PROP_PREFIX + ".password");

        public String propertyKey;
        Properties(String propertyKey){
            this.propertyKey = propertyKey;
        }
    }

    private static final String MAIN_CONFIG_FILE_NAME = "configuration.properties";
    private static CombinedConfiguration combinedConfiguration;

    static {
        final Configurations configurations = new Configurations();
        combinedConfiguration = new CombinedConfiguration();
        try {
            combinedConfiguration.addConfiguration(new SystemConfiguration());
            combinedConfiguration.addConfiguration(configurations.properties(MAIN_CONFIG_FILE_NAME));
        } catch (ConfigurationException ex) {
            throw new RuntimeException(ex);
        }
    }

    public static String getString(Properties property) {
        return combinedConfiguration.getString(property.propertyKey);
    }

    public static Long getLong(Properties property) {
        return combinedConfiguration.getLong(property.propertyKey);
    }

}