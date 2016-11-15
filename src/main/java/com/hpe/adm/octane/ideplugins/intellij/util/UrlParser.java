package com.hpe.adm.octane.ideplugins.intellij.util;

import com.hpe.adm.octane.ideplugins.services.ConnectionSettings;

import java.net.URL;

public class UrlParser {
    private static URL site;

    public UrlParser() {
    }

    public static ConnectionSettings resolveConnectionSettings(String url, String userName, String password) {
        ConnectionSettings connectionSettings = new ConnectionSettings();
        try {
            site = new URL(url);
            if (null != site.getQuery()) {
                String baseUrl = site.getProtocol() + "://" + site.getHost();
                String[] split = site.getQuery().split("/");
                Long sharedspaceId = Long.valueOf(split[0].substring(split[0].indexOf("p=") + 2));
                Long workspaceId = Long.valueOf(split[1]);

                connectionSettings.setBaseUrl(baseUrl);
                connectionSettings.setSharedSpaceId(sharedspaceId);
                connectionSettings.setWorkspaceId(workspaceId);
                connectionSettings.setUserName(userName);
                connectionSettings.setPassword(password);
            }

        } catch (Exception e) {
        }

        return connectionSettings;
    }

}
