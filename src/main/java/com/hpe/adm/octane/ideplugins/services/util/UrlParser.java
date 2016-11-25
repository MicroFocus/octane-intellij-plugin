package com.hpe.adm.octane.ideplugins.services.util;

import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.validator.UrlValidator;

import java.net.URL;

import static com.hpe.adm.octane.ideplugins.intellij.util.Constants.CORRECT_URL_FORMAT_MESSAGE;
import static com.hpe.adm.octane.ideplugins.intellij.util.Constants.INVALID_URL_FORMAT_MESSAGE;

public class UrlParser {

    // Only works for a html enabled JLabel,
    // TODO: use error codes or subtypes of ServiceException instead of passing a message
    private static final String LINE_BREAK = "<br>";

    public static ConnectionSettings resolveConnectionSettings(String url, String userName, String password) throws ServiceException {

        ConnectionSettings connectionSettings = new ConnectionSettings();

        //Try to validate the url before
        String[] schemes = {"http", "https"};
        UrlValidator urlValidator = new UrlValidator(schemes);
        if (!urlValidator.isValid(url)) {
            throw new ServiceException(INVALID_URL_FORMAT_MESSAGE
                    + LINE_BREAK
                    + CORRECT_URL_FORMAT_MESSAGE);
        }

        //This check is not redundant, above does some extra things
        URL siteUrl;
        try {
            siteUrl = new URL(url);
        } catch (Exception ex){
            throw new ServiceException(INVALID_URL_FORMAT_MESSAGE
                    + CORRECT_URL_FORMAT_MESSAGE);
        }

        if (null == siteUrl.getQuery()) {
            throw new ServiceException("Missing query parameters."
                    + LINE_BREAK
                    + CORRECT_URL_FORMAT_MESSAGE);
        } else {

            try {

                String baseUrl;
                Long sharedspaceId;
                Long workspaceId;

                baseUrl = siteUrl.getProtocol() + "://" + siteUrl.getHost();
                //Add port if not the default
                if(siteUrl.getPort() != 80 && siteUrl.getPort() != -1){
                    baseUrl += ":" + siteUrl.getPort();
                }

                String[] split = siteUrl.getQuery().split("/");
                sharedspaceId = Long.valueOf(split[0].substring(split[0].indexOf("p=") + 2));
                workspaceId = Long.valueOf(split[1]);

                if(sharedspaceId<0)
                    throw new Exception();

                if(workspaceId<0)
                    throw new Exception();


                connectionSettings.setBaseUrl(baseUrl);
                connectionSettings.setSharedSpaceId(sharedspaceId);
                connectionSettings.setWorkspaceId(workspaceId);


            } catch (Exception ex) {
                throw new ServiceException("Could not get sharedspace/workspace ids from URL. "
                        + LINE_BREAK
                        + CORRECT_URL_FORMAT_MESSAGE);
            }

        }

        if(StringUtils.isEmpty(userName)){
            throw new ServiceException("Username cannot be blank.");
        }
        connectionSettings.setUserName(userName);

        if(StringUtils.isEmpty(userName)){
            throw new ServiceException("Password cannot be blank.");
        }
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
                + "/?"
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
