package com.hpe.adm.octane.ideplugins.services;

import com.hpe.adm.nga.sdk.Octane;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.SdkUtil;

import java.net.HttpURLConnection;
import java.net.URL;
import java.net.UnknownHostException;

public class TestService extends ServiceBase{

    private Octane getOctane(ConnectionSettings connectionSettings){
        Octane nga = new Octane
                    .Builder(new UserAuthorisation(connectionSettings.getUserName(), connectionSettings.getPassword()))
                    .Server(connectionSettings.getBaseUrl())
                    .sharedSpace(connectionSettings.getSharedSpaceId())
                    .workSpace(connectionSettings.getWorkspaceId())
                    .build();
        return nga;
    }

    private void testHttpConnection(ConnectionSettings connectionSettings) throws ServiceException {
        try {
            HttpURLConnection connection = (HttpURLConnection) new URL(connectionSettings.getBaseUrl()).openConnection();
            connection.setRequestMethod("HEAD");
            int responseCode = connection.getResponseCode();
            if (responseCode != 200) {
                // Not OK.
            }
        } catch (Exception ex){
            throw new ServiceException("HTTP connection to url: " + connectionSettings.getBaseUrl() + " failed.");
        }
    }

    /**
     * Check if the current connection settings are valid
     */
    public void testConnection(ConnectionSettings connectionSettings) throws ServiceException {

        //Try basic http connection first
        testHttpConnection(connectionSettings);

        try{
            //Try to fetch the backlog root
            getOctane(connectionSettings).entityList(Entity.DEFECT.getApiEntityName()).get().execute();
        } catch (Exception ex){
            String message = null;

            if(ex instanceof OctaneException){
                message = SdkUtil.getMessageFromOctaneException((OctaneException)ex);
            }
            else if(ex instanceof UnknownHostException){
                message = "Failed to connect to host: " + ex.getMessage();
            }
            //Default
            if(message == null) {
                message = ex.getMessage();
            }

            throw new ServiceException(message, ex);
        }
    }

}