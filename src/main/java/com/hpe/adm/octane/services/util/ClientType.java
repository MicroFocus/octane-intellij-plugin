package com.hpe.adm.octane.services.util;
/**
 * <p>
 * Enum of ClientType and their AccessLevelValue
 * <p>
 * Each client needs to add itself to this ClientType enum
 * A client that wants access to PROTECTED resources needs to define its ClientType AccessLevelValue as AccessLevelValue.PROTECTED
 * and send its own client type in the HPECLIENTTYPE header.
 * A client that wants access to PUBLIC_INTERNAL resources needs to define its ClientType AccessLevelValue as AccessLevelValue.PUBLIC_INTERNAL
 * and send its own client type in the HPECLIENTTYPE header.
 */
public enum ClientType {

    // Production Client Types
    HPE_PUBLIC_API(AccessLevelValue.PUBLIC),
    HPE_SWAGGER_API(AccessLevelValue.PUBLIC),
    HPE_CI_CLIENT(AccessLevelValue.PROTECTED),
    HPE_SYNCHRONIZER(AccessLevelValue.PROTECTED),
    HPE_MQM_UI(AccessLevelValue.PROTECTED),
    HPE_MQM_MOBILE(AccessLevelValue.PROTECTED),
    HPE_REST_TESTS_TEMP(AccessLevelValue.PROTECTED),
    HPE_MQM_PLUGIN_UI(AccessLevelValue.PROTECTED),
    HPE_REST_API_TECH_PREVIEW(AccessLevelValue.PROTECTED),
    HPE_SERVICES(AccessLevelValue.PROTECTED),

    //for the ppm team (in hpe), currently used for com.hp.mqm.rest.platformservices.TimesheetResouce
    HPE_PPM(AccessLevelValue.PUBLIC_INTERNAL),

    // Client Types for IT
    IT_PUBLIC(AccessLevelValue.PUBLIC),
    IT_PUBLIC_INTERNAL(AccessLevelValue.PUBLIC_INTERNAL),
    IT_PROTECTED(AccessLevelValue.PROTECTED);


    public boolean isUIClient() {
        return this == HPE_MQM_UI || this == HPE_MQM_MOBILE;
    }

    private final AccessLevelValue accessLevelValue;

    ClientType(AccessLevelValue accessLevelValue) {
        this.accessLevelValue = accessLevelValue;
    }

    public AccessLevelValue getAccessLevelValue() {
        return this.accessLevelValue;
    }
}