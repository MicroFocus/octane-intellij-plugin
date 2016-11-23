package com.hpe.adm.octane.ideplugins.services.util;

import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import org.apache.commons.validator.UrlValidator;

import java.net.URL;

public class UrlParser {

    public static ConnectionSettings resolveConnectionSettings(String url, String userName, String password) throws ServiceException {

        ConnectionSettings connectionSettings = new ConnectionSettings();

        //Try to validate the url before
        String[] schemes = {"http","https"};
        UrlValidator urlValidator = new UrlValidator(schemes);
        if (!urlValidator.isValid(url)) {
            throw new ServiceException("Given server url is not valid.");
        }
        try {
            URL site = new URL(url);
            if (null != site.getQuery()) {
                String baseUrl = site.getProtocol() + "://" + site.getHost();

                //Add port if not the default
                if(site.getPort() != 80 && site.getPort() != -1){
                    baseUrl += ":" + site.getPort();
                }

                String[] split = site.getQuery().split("/");
                Long sharedspaceId = Long.valueOf(split[0].substring(split[0].indexOf("p=") + 2));
                Long workspaceId = Long.valueOf(split[1]);

                connectionSettings.setBaseUrl(baseUrl);
                connectionSettings.setSharedSpaceId(sharedspaceId);
                connectionSettings.setWorkspaceId(workspaceId);
            }

        } catch (Exception e) {
            throw new ServiceException("Failed to retrieve sharedspace and workspace from given server url.");
        }

        connectionSettings.setUserName(userName);
        connectionSettings.setPassword(password);

        return connectionSettings;
    }

    /**
     * Create an octane url from a connection settings object
     * @param connectionSettings {@link ConnectionSettings} object
     * @return octane browser url or null if if any of the req. fields are missing (base url, workspace id, shared space id)
     */
    public static String createUrlFromConnectionSettings(ConnectionSettings connectionSettings){

        if(connectionSettings.getBaseUrl() == null ||
                connectionSettings.getWorkspaceId() == null ||
                connectionSettings.getSharedSpaceId() == null) {
            return  null;
        }

        return connectionSettings.getBaseUrl()
                + "/ui/?"
                + "p=" + connectionSettings.getSharedSpaceId()
                + "/" + connectionSettings.getWorkspaceId();
    }

    public static String removeHash(String url){
        if(url.contains("#")){
            return url.substring(0,url.indexOf("#"));
        }
        return url;
    }

}
