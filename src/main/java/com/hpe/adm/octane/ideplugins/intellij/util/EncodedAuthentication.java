package com.hpe.adm.octane.ideplugins.intellij.util;

import com.hpe.adm.nga.sdk.authentication.SimpleUserAuthentication;

public abstract class EncodedAuthentication extends SimpleUserAuthentication {

    public EncodedAuthentication(String userName, String password) {
        super(userName, password);
    }

    public abstract String getPassword();
}
