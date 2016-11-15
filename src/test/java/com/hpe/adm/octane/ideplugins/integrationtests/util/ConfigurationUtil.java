package com.hpe.adm.octane.ideplugins.integrationtests.util;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

public class ConfigurationUtil {

    private static final String PROP_PREFIX = "sdk";
    public enum PropertyKeys {
        URL(PROP_PREFIX + ".url"),
        SHAREDSPACE(PROP_PREFIX + ".sharedSpaceId"),
        WORKSPACE(PROP_PREFIX + ".workspaceId"),
        USERNAME(PROP_PREFIX + ".username"),
        PASSWORD(PROP_PREFIX + ".password");

        public String propertyKey;
        PropertyKeys(String propertyKey){
            this.propertyKey = propertyKey;
        }
    }

    private static final String MAIN_CONFIG_FILE_NAME = "configuration.properties";
    private static final Properties properties = new Properties();
    static {
        try {
            properties.load(ConfigurationUtil.class.getClassLoader().getResourceAsStream(MAIN_CONFIG_FILE_NAME));
        } catch (IOException ex){
            throw new RuntimeException("Error occured while loading config file: " + MAIN_CONFIG_FILE_NAME + ", " + ex);
        }
    }

    public static String getString(PropertyKeys property) {
        return properties.getProperty(property.propertyKey) ;
    }

    public static Long getLong(PropertyKeys property) {
        return Long.valueOf(properties.getProperty(property.propertyKey));
    }

}