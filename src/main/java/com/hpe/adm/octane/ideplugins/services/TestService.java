package com.hpe.adm.octane.ideplugins.services;

import com.hpe.adm.nga.sdk.NGA;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.nga.sdk.exception.NgaException;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.SdkUtil;

import java.net.HttpURLConnection;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.concurrent.*;

public class TestService extends ServiceBase{

    private NGA getNGA(ConnectionSettings connectionSettings){
        NGA nga = new NGA
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

        final ExecutorService executor = Executors.newSingleThreadExecutor();
        final Future<Void> future = executor.submit(()->{
            try{
                //Try to fetch the backlog root
                getNGA(connectionSettings).entityList(Entity.WORK_ITEM.getApiEntityName()).at(1001).get().execute();
            } catch (Exception ex){
                String message = null;
                if(ex instanceof NgaException){
                    message = SdkUtil.getMessageFromNgaException((NgaException)ex);
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
            return null;
        });

        executor.shutdown(); // This does not cancel the already-scheduled task.
        try {
            future.get(2, TimeUnit.SECONDS);
        } catch (ExecutionException e) {
            throw new ServiceException(e.getCause());

        } catch (InterruptedException | TimeoutException e) {
            //remove this if you do not want to cancel the job in progress
            //or set the argument to 'false' if you do not want to interrupt the thread
            future.cancel(true);

            //Probably invalid credentials, sdk issues
            throw new ServiceException("Username and password combination invalid");
        }
    }

}